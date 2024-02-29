{-# LANGUAGE RecordWildCards #-}

module UI.Benchmark
  ( selectedMain
  ) where

import Prelude hiding (take)

import Control.Monad (forM_)
import Data.Vector.Unboxed (take)
import Data.Word (Word64)
import System.Environment (getArgs)
import Text.Printf (printf)

import Core (CoreState(..), SalisState(..), new, step)

selectedMain :: IO ()
selectedMain = do
  args <- getArgs
  let x = (read $ head args :: Word64)
  let n = (read $ args !! 1 :: Int)
  let SalisState {..} = step n $ new x
  printf "Salis speed test\n"
  printf "Using seed        : 0x%x\n" x
  printf "Will run N cycles : %i\n" n
  printf "\n"
  printf "iters : %i\n" iters
  printf "syncs : %i\n" syncs
  forM_ cores $ \CoreState {..} -> do
    printf "\n"
    printf "loop : %s\n" $ show loop
    printf "mall : %s\n" $ show mall
    printf "pnum : %s\n" $ show pnum
    printf "pcap : %s\n" $ show pcap
    printf "pfst : %s\n" $ show pfst
    printf "plst : %s\n" $ show plst
    printf "pcur : %s\n" $ show pcur
    printf "psli : %s\n" $ show psli
    printf "muta : %s\n" $ show muta
    printf "mvec : %s\n" $ show $ take 16 mvec
    printf "pvec : %s\n" $ show $ take 16 pvec
