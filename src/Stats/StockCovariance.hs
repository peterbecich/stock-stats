module Stats.StockCovariance where

import Prelude

import Control.Monad
import Control.Concurrent
import Statistics.Sample
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
import DB.Redis (getRedisConnection, closeRedisConnection)

-- https://hackage.haskell.org/package/statistics-0.14.0.2/docs/Statistics-Sample.html#v:correlation

confPath = "conf/stats.yaml"


-- TODO limit ticks to given number
--zip ticks by timestamp; pairwise timestamps should equal
pairCovariance :: Redis.Connection
               -> Postgres.Connection
               -> Stock
               -> Stock
               -> Int
               -> IO ()
pairCovariance _ _ stockA stockB limit = do
  threadDelay 10000000

  psqlConn <- getPsqlConnection confPath
  redisConn <- getRedisConnection confPath

  
  stockAClosingPrices <- V.fromList <$> getStockClose psqlConn (stockId stockA)
  stockBClosingPrices <- V.fromList <$> getStockClose psqlConn (stockId stockB)
  let
    closingZipped :: V.Vector (Double, Double)
    closingZipped = V.zip stockAClosingPrices stockBClosingPrices
    cov :: Double
    cov = covariance closingZipped

    pairCovariance = PairCovariance stockA stockB cov

  putStrLn $ (symbol stockA) ++ " - " ++ (symbol stockB) ++ " covariance: " ++ show cov
  
  _ <- Redis.runRedis redisConn (setPairCovariance pairCovariance)


  closePsqlConnection psqlConn
  _ <- closeRedisConnection redisConn

  return ()


pairCovarianceStocks :: Redis.Connection
                     -> Postgres.Connection
                     -> [Stock]
                     -> IO ()
pairCovarianceStocks redisConn psqlConn stocks = do
  stockPairs <- pure $ do -- list monad
    stockA <- stocks
    stockB <- stocks
    return (stockA, stockB)
  putStrLn $ "pairs of stocks: " ++ show (length stockPairs)

  mapM_ (\(stockA, stockB) -> void $ forkIO $ pairCovariance redisConn psqlConn stockA stockB 100) stockPairs

  return ()

pairCovarianceNStocks :: Redis.Connection
                     -> Postgres.Connection
                     -> [Stock]
                     -> Int
                     -> IO ()
pairCovarianceNStocks redisConn psqlConn stocks limit =
  pairCovarianceStocks redisConn psqlConn (take limit stocks)

pairCovarianceExample = do
  psqlConn <- getPsqlConnection confPath
  redisConn <- getRedisConnection confPath

  stocks <- getStocks psqlConn

  pairCovarianceNStocks redisConn psqlConn stocks 256

  closePsqlConnection psqlConn
  _ <- closeRedisConnection redisConn
  return ()

pairCovarianceAllExample = do
  psqlConn <- getPsqlConnection confPath
  redisConn <- getRedisConnection confPath

  stocks <- getStocks psqlConn

  pairCovarianceStocks redisConn psqlConn stocks

  closePsqlConnection psqlConn
  _ <- closeRedisConnection redisConn
  return ()


accelerateDiagnosticsUUID :: UUID
(Just accelerateDiagnosticsUUID) = fromString "a3b88460-d455-451a-92d1-c85109a9bfb1"

stockCovarianceExample = do
  conn <- getPsqlConnection confPath
  flowersPrices <- getStockClose conn flowersUUID
  diagPrices <- getStockClose conn accelerateDiagnosticsUUID

  putStrLn $ (show (length flowersPrices)) ++ " flowers ticks"
  putStrLn $ (show (length diagPrices)) ++ " AccelerateDiagnostics ticks"

  let
    flowersV = V.fromList flowersPrices
    diagV = V.fromList diagPrices
    flowersDiagZipped = V.zip flowersV diagV
  
    cov = covariance flowersDiagZipped
    -- cov' = covariance flowersPrices diagPrices

  putStrLn $ "covariance: " ++ (show cov)


  putStrLn "done"
  



