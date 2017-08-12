{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion.Main
import qualified Data.Vector.Storable                                   as DVS
import           Data.Word
import           HaskellWorks.Data.RankSelect.Base

setupEnvVector :: Int -> IO (DVS.Vector Word64)
setupEnvVector n = return $ DVS.fromList (take n (cycle [maxBound, 0]))

benchRankSelect :: [Benchmark]
benchRankSelect =
  [ env (return ()) $ \_ -> bgroup "8-bit"
    [ bench "Rank - Once"   (whnf (rank1    (0x55 :: Word8)) 8)
    , bench "Select - Once" (whnf (select1  (0x55 :: Word8)) 8)
    ]
  , env (return ()) $ \_ -> bgroup "16-bit"
    [ bench "Rank - Once"   (whnf (rank1    (0x5555 :: Word16)) 16)
    , bench "Select - Once" (whnf (select1  (0x5555 :: Word16)) 16)
    ]
  , env (return ()) $ \_ -> bgroup "32-bit"
    [ bench "Rank - Once"   (whnf (rank1    (0x55555555 :: Word32)) 32)
    , bench "Select - Once" (whnf (select1  (0x55555555 :: Word32)) 32)
    ]
  , env (return ()) $ \_ -> bgroup "64-bit"
    [ bench "Rank - Once"   (whnf (rank1    (0x5555555555555555 :: Word64)) 64)
    , bench "Select - Once" (whnf (select1  (0x5555555555555555 :: Word64)) 64)
    ]
  , env (setupEnvVector 1000000) $ \bv -> bgroup "64-bit vector"
    [ bench "Rank - Once"   (whnf (rank1    bv) 1)
    , bench "Select - Once" (whnf (select1  bv) 1)
    , bench "Rank - Many"   (nf   (map (rank1 bv)) [0, 1000..10000000])
    ]
  ]

main :: IO ()
main = defaultMain benchRankSelect
