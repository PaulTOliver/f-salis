{-# LANGUAGE Strict #-}

module Config
  ( coreCount
  , coreSize
  , syncInterval
  ) where

import Data.Bits ((!<<.))
import Data.Word (Word64)

coreCount :: Int
coreCount = 2

coreSize :: Word64
coreSize = 1 !<<. 20

syncInterval :: Int
syncInterval = 1 !<<. 20
