{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Arch.Dummy
  ( Process(..)
  , procAncestor
  , procMemBlocks
  , procRep
  , procSliceM
  , procStepM
  , bimapMnemonic
  , bimapSymbol
  ) where

import Control.Monad.ST (ST)
import Data.Bimap (Bimap, fromList)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified Data.Vector.Unboxed.Mutable as M (STVector)
import Data.Word (Word64, Word8)
import Text.Printf (printf)

data Process =
  Process
  deriving (Show)

$(derivingUnbox "Process" [t|Process -> Word64|] [|const 1|] [|const Process|])

procAncestor :: Process
procAncestor = Process

procMemBlocks :: Process -> [(Word64, Word64)]
procMemBlocks _ = []

procRep :: Process -> [(String, String)]
procRep _ = [("dmmy", "----")]

procSliceM :: M.STVector s Process -> M.STVector s Word8 -> Word64 -> ST s Word64
procSliceM _ _ _ = return 1

procStepM :: M.STVector s Process -> M.STVector s Word8 -> Word64 -> ST s (Maybe Process)
procStepM _ _ _ = return Nothing

bimapMnemonic :: Bimap Word8 [String]
bimapMnemonic = fromList [(i, ["dmmy", printf "0x%x" i]) | i <- [0 .. 0xff]]

bimapSymbol :: Bimap Word8 Char
bimapSymbol =
  fromList
    $ zip [0 .. 0xff]
    $ concat
        [ "⠀⠁⠂⠃⠄⠅⠆⠇⡀⡁⡂⡃⡄⡅⡆⡇⠈⠉⠊⠋⠌⠍⠎⠏⡈⡉⡊⡋⡌⡍⡎⡏⠐⠑⠒⠓⠔⠕⠖⠗⡐⡑⡒⡓⡔⡕⡖⡗⠘⠙⠚⠛⠜⠝⠞⠟⡘⡙⡚⡛⡜⡝⡞⡟"
        , "⠠⠡⠢⠣⠤⠥⠦⠧⡠⡡⡢⡣⡤⡥⡦⡧⠨⠩⠪⠫⠬⠭⠮⠯⡨⡩⡪⡫⡬⡭⡮⡯⠰⠱⠲⠳⠴⠵⠶⠷⡰⡱⡲⡳⡴⡵⡶⡷⠸⠹⠺⠻⠼⠽⠾⠿⡸⡹⡺⡻⡼⡽⡾⡿"
        , "⢀⢁⢂⢃⢄⢅⢆⢇⣀⣁⣂⣃⣄⣅⣆⣇⢈⢉⢊⢋⢌⢍⢎⢏⣈⣉⣊⣋⣌⣍⣎⣏⢐⢑⢒⢓⢔⢕⢖⢗⣐⣑⣒⣓⣔⣕⣖⣗⢘⢙⢚⢛⢜⢝⢞⢟⣘⣙⣚⣛⣜⣝⣞⣟"
        , "⢠⢡⢢⢣⢤⢥⢦⢧⣠⣡⣢⣣⣤⣥⣦⣧⢨⢩⢪⢫⢬⢭⢮⢯⣨⣩⣪⣫⣬⣭⣮⣯⢰⢱⢲⢳⢴⢵⢶⢷⣰⣱⣲⣳⣴⣵⣶⣷⢸⢹⢺⢻⢼⢽⢾⢿⣸⣹⣺⣻⣼⣽⣾⣿"
        ]
