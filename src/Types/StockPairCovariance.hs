{-# LANGUAGE DeriveGeneric #-}

module Types.StockPairCovariance where

import Data.Time.Clock

import Types.Stock

data PairCovariance = PairCovariance { stockA :: Stock
                                     , stockB :: Stock
--                                     , interval :: Int -- hours
--                                     , endTimestamp :: UTCTime
                                     , cov :: Double
                                     }
