{-# LANGUAGE OverloadedStrings #-}

module Types.StockPairCovariance.Redis where

import Database.Redis
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
    
    key :: String
    key = case (keyA <= keyB) of
      True -> (show keyA)++(show keyB)
      False -> (show keyB)++(show keyA)

    value :: String
    value = (show (cov pairCovariance))




