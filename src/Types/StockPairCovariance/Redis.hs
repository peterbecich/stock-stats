{-# LANGUAGE OverloadedStrings #-}

module Types.StockPairCovariance.Redis where

import Database.Redis
import Data.Either
import Data.UUID
import Data.Ord
import qualified Data.ByteString.Char8 as BS

import Types.StockPairCovariance
import qualified Types.Stock as Stock
import DB.Redis

setPairCovariance :: PairCovariance -> Redis (Either Reply Status)
setPairCovariance pairCovariance = set (BS.pack ("pairCovariance:"++key)) (BS.pack value)
  where
    keyA :: UUID
    keyA = Stock.stockId (stockA pairCovariance)

    keyB :: UUID
    keyB = Stock.stockId (stockB pairCovariance)

    -- lower key goes first
    key :: String
    key = case (keyA <= keyB) of
      True -> (show keyA)++":"++(show keyB)
      False -> (show keyB)++":"++(show keyA)

    value :: String
    value = (show (cov pairCovariance))

retrievePairCovariance' :: UUID
                        -> UUID
                        -> Redis (Either Reply (Maybe Double))
retrievePairCovariance' keyA keyB = do
  let
    -- lower key goes first
    key :: String
    key = case (keyA <= keyB) of
      True -> (show keyA)++":"++(show keyB)
      False -> (show keyB)++":"++(show keyA)
  
  -- TODO make safe!
  (Right mBS) <- get (BS.pack (key))

  let
    -- TODO make safe!
    bs :: BS.ByteString
    (Just bs) = mBS

    s :: String
    s = BS.unpack bs

    d :: Double
    d = read s

  return $ Right $ Just d
  

retrievePairCovariance :: Stock.Stock
                       -> Stock.Stock
                       -> Redis (Either Reply (Maybe Double))
retrievePairCovariance stockA stockB = retrievePairCovariance' keyA keyB
  where
    keyA :: UUID
    keyA = Stock.stockId stockA

    keyB :: UUID
    keyB = Stock.stockId stockB


exampleDoubleS :: String
exampleDoubleS = "-7.63023094369823e-3"

exampleDouble :: Double
exampleDouble = read exampleDoubleS
