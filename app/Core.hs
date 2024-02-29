{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Core
  ( CoreState(..)
  , SalisState(..)
  , new
  , step
  ) where

import Control.Monad (replicateM, replicateM_, when)
import Control.Monad.Par (NFData, parMap, runPar)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (State, evalState, state)
import Data.Bits ((!<<.), (!>>.), (.^.), (.|.), complementBit)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V (Vector, fromList)
import qualified Data.Vector.Unboxed as U (Vector, fromList, replicate, thaw, unsafeFreeze)
import qualified Data.Vector.Unboxed.Mutable as M (STVector, unsafeModify, unsafeRead)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import System.Random.SplitMix (SMGen, mkSMGen, nextWord64)

import Arch.Dummy (Process(..), procAncestor, procSliceM, procStepM)
import Config (coreCount, coreSize, syncInterval)

mvecInit :: U.Vector Word8
mvecInit = U.replicate (fromIntegral coreSize) 0

mvecLoop :: Word64 -> Word64
mvecLoop = (`mod` coreSize)

mvecMutateM :: M.STVector s Word8 -> Word64 -> Word64 -> ST s ()
mvecMutateM mvecM a b = do
  let a' = fromIntegral $ mvecLoop a
  let b' = fromIntegral b
  when (b /= 0) $ M.unsafeModify mvecM (`complementBit` b') a'

mutaSeeds :: State SMGen [[Word64]]
mutaSeeds = replicateM coreCount $ replicateM 4 $ state nextWord64

mutaInit :: Word64 -> [[Word64]]
mutaInit 0 = replicate coreCount $ replicate 4 0
mutaInit s = evalState mutaSeeds $ mkSMGen s

mutaR64 :: Word64 -> Int -> Word64
mutaR64 x k = (x !<<. k) .|. (x !>>. (64 - k))

mutaXorM :: M.STVector s Word64 -> Int -> Int -> ST s ()
mutaXorM mutaM a b = M.unsafeRead mutaM b >>= \b' -> M.unsafeModify mutaM (.^. b') a

mutaNextM :: M.STVector s Word64 -> ST s Word64
mutaNextM mutaM = do
  s1 <- M.unsafeRead mutaM 1
  let r = mutaR64 (s1 * 5) 7 * 9
  let t = s1 !<<. 17
  mutaXorM mutaM 2 0
  mutaXorM mutaM 3 1
  mutaXorM mutaM 1 2
  mutaXorM mutaM 0 3
  M.unsafeModify mutaM (.^. t) 2
  M.unsafeModify mutaM (`mutaR64` 45) 3
  return r

mutaCosmicRayM :: M.STVector s Word64 -> M.STVector s Word8 -> ST s ()
mutaCosmicRayM mutaM mvecM = do
  a <- mutaNextM mutaM
  b <- mutaNextM mutaM
  mvecMutateM mvecM a (b `mod` 8)

pvecInit :: U.Vector Process
pvecInit = U.fromList [procAncestor]

pvecBirthM :: M.STVector s Process -> Maybe Process -> ST s ()
pvecBirthM _ _ = return ()

data CoreState = CoreState
  { loop :: Word64
  , mall :: Word64
  , pnum :: Word64
  , pcap :: Word64
  , pfst :: Word64
  , plst :: Word64
  , pcur :: Word64
  , psli :: Word64
  , muta :: U.Vector Word64
  , mvec :: U.Vector Word8
  , pvec :: U.Vector Process
  } deriving (Generic, NFData)

data CoreStateM s = CoreStateM
  { loopM :: STRef s Word64
  , mallM :: STRef s Word64
  , pnumM :: STRef s Word64
  , pcapM :: STRef s Word64
  , pfstM :: STRef s Word64
  , plstM :: STRef s Word64
  , pcurM :: STRef s Word64
  , psliM :: STRef s Word64
  , mutaM :: M.STVector s Word64
  , mvecM :: M.STVector s Word8
  , pvecM :: M.STVector s Process
  }

coreInit :: [Word64] -> CoreState
coreInit s =
  CoreState
    { loop = 0
    , mall = 0
    , pnum = 1
    , pcap = 1
    , pfst = 0
    , plst = 0
    , pcur = 0
    , psli = 0
    , muta = U.fromList s
    , mvec = mvecInit
    , pvec = pvecInit
    }

coreCycle :: Int -> CoreState -> CoreState
coreCycle n c = runST $ coreThawM c >>= (\c' -> replicateM_ n (coreStepM c') >> coreFreezeM c')

coreThawM :: CoreState -> ST s (CoreStateM s)
coreThawM CoreState {..} =
  CoreStateM
    <$> newSTRef loop
    <*> newSTRef mall
    <*> newSTRef pnum
    <*> newSTRef pcap
    <*> newSTRef pfst
    <*> newSTRef plst
    <*> newSTRef pcur
    <*> newSTRef psli
    <*> U.thaw muta
    <*> U.thaw mvec
    <*> U.thaw pvec

coreStepM :: CoreStateM s -> ST s ()
coreStepM c@CoreStateM {..} = do
  psli' <- readSTRef psliM
  pcur' <- readSTRef pcurM
  if psli' /= 0
    then do
      modifySTRef' psliM $ subtract 1
      child <- procStepM pvecM mvecM pcur'
      pvecBirthM pvecM child
    else do
      plst' <- readSTRef plstM
      if pcur' /= plst'
        then do
          modifySTRef' pcurM succ
          pnext <- readSTRef pcurM
          pnsli <- procSliceM pvecM mvecM pnext
          writeSTRef psliM pnsli
          coreStepM c
        else do
          pnext <- readSTRef pfstM
          pnsli <- procSliceM pvecM mvecM pnext
          modifySTRef' loopM succ
          writeSTRef pcurM pnext
          writeSTRef psliM pnsli
          mutaCosmicRayM mutaM mvecM
          coreStepM c

coreFreezeM :: CoreStateM s -> ST s CoreState
coreFreezeM CoreStateM {..} =
  CoreState
    <$> readSTRef loopM
    <*> readSTRef mallM
    <*> readSTRef pnumM
    <*> readSTRef pcapM
    <*> readSTRef pfstM
    <*> readSTRef plstM
    <*> readSTRef pcurM
    <*> readSTRef psliM
    <*> U.unsafeFreeze mutaM
    <*> U.unsafeFreeze mvecM
    <*> U.unsafeFreeze pvecM

data SalisState = SalisState
  { iters :: Int
  , syncs :: Int
  , cores :: V.Vector CoreState
  }

new :: Word64 -> SalisState
new s = SalisState 0 0 $ V.fromList $ map coreInit $ mutaInit s

step :: Int -> SalisState -> SalisState
step n s@SalisState {iters} = stepAndSync (syncInterval - iters `mod` syncInterval) n s

stepAndSync :: Int -> Int -> SalisState -> SalisState
stepAndSync d n =
  if n >= d
    then stepAndSync syncInterval (n - d) . sync . stepCores d
    else stepCores n

stepCores :: Int -> SalisState -> SalisState
stepCores n s@SalisState {iters, cores} = s {iters = n + iters, cores = runPar $ parMap (coreCycle n) cores}

sync :: SalisState -> SalisState
sync s@SalisState {syncs} = s {syncs = succ syncs}
