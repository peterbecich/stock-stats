module Stats.StockCovariance where

import Prelude

import Control.Monad
import Control.Concurrent
import Statistics.Sample
import Data.List
import qualified Data.Vector as V
import Data.UUID

import qualified Database.Redis as Redis (Connection, runRedis)
import qualified Database.PostgreSQL.Simple as Postgres (Connection)

import Types.Stock
import Types.Stock.Psql
import Types.Tick
import Types.Tick.Psql

import Types.StockPairCovariance
import Types.StockPairCovariance.Redis

import DB.Psql
import DB.Redis

-- https://hackage.haskell.org/package/statistics-0.14.0.2/docs/Statistics-Sample.html#v:correlation

confPath = "conf/stats.yaml"


-- TODO limit ticks to given number
--zip ticks by timestamp; pairwise timestamps should equal
pairCovariance :: RedisPool
               -> PostgresPool
               -> Stock
               -> Stock
               -> Int
               -> IO ()
pairCovariance redisPool psqlPool stockA stockB limit = do
  stockAClosingPrices <- V.fromList <$> getStockClose psqlPool (stockId stockA)
  stockBClosingPrices <- V.fromList <$> getStockClose psqlPool (stockId stockB)
  let
    closingZipped :: V.Vector (Double, Double)
    closingZipped = V.zip stockAClosingPrices stockBClosingPrices

    cov :: Double
    cov = covariance closingZipped

    pairCovariance = PairCovariance stockA stockB cov

  putStrLn $ (symbol stockA) ++ " - " ++ (symbol stockB) ++ " covariance: " ++ show cov
  
  runRedisPool redisPool (setPairCovariance pairCovariance)
  
  return ()

-- clearly abysmally slow
-- tickers are pulled out the DB too many times
pairCovarianceStocks :: RedisPool
                     -> PostgresPool
                     -> [Stock]
                     -> IO ()
pairCovarianceStocks redisConn psqlPool stocks = do
  stockPairs <- pure $ do -- list monad
    stockA <- stocks
    stockB <- stocks
    return (stockA, stockB)
  let stockPairs' = nub stockPairs
  putStrLn $ "pairs of stocks: " ++ show (length stockPairs)

  mapM_ (\(stockA, stockB) -> void $ pairCovariance redisConn psqlPool stockA stockB 100) stockPairs
 
  return ()

pairCovarianceNStocks :: RedisPool
                      -> PostgresPool
                      -> [Stock]
                      -> Int
                      -> IO ()
pairCovarianceNStocks redisPool psqlPool stocks limit =
  pairCovarianceStocks redisPool psqlPool (take limit stocks)

accelerateDiagnosticsUUID :: UUID
(Just accelerateDiagnosticsUUID) = fromString "a3b88460-d455-451a-92d1-c85109a9bfb1"

-- stockCovarianceExample = do
--   conn <- getPsqlConnection confPath
--   flowersPrices <- getStockClose conn flowersUUID
--   diagPrices <- getStockClose conn accelerateDiagnosticsUUID

--   putStrLn $ (show (length flowersPrices)) ++ " flowers ticks"
--   putStrLn $ (show (length diagPrices)) ++ " AccelerateDiagnostics ticks"

--   let
--     flowersV = V.fromList flowersPrices
--     diagV = V.fromList diagPrices
--     flowersDiagZipped = V.zip flowersV diagV
  
--     cov = covariance flowersDiagZipped
--     -- cov' = covariance flowersPrices diagPrices

--   putStrLn $ "covariance: " ++ (show cov)


--   putStrLn "done"
  



