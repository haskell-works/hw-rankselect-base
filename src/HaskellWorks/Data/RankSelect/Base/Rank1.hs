{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.RankSelect.Base.Rank1
    ( Rank1(..)
    ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.ElemFixedBitSize
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Positioning
import Prelude                                   as P

import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

{-# ANN module ("HLint: Ignore Reduce duplication"  :: String) #-}

class Rank1 v where
  rank1 :: v -> Count -> Count

deriving instance Rank1 a => Rank1 (BitShown a)

instance Rank1 Word8 where
  rank1 _ 0 = 0
  rank1 v i = popCount1 (v .&. ((1 .<. fromIntegral i) - 1))
  {-# INLINABLE rank1 #-}

instance Rank1 Word16 where
  rank1 _ 0 = 0
  rank1 v i = popCount1 (v .&. ((1 .<. fromIntegral i) - 1))
  {-# INLINABLE rank1 #-}

instance Rank1 Word32 where
  rank1 _ 0 = 0
  rank1 v i = popCount1 (v .&. ((1 .<. fromIntegral i) - 1))
  {-# INLINABLE rank1 #-}

instance Rank1 Word64 where
  rank1 _ 0 = 0
  rank1 v i = popCount1 (v .&. ((1 .<. fromIntegral i) - 1))
  {-# INLINABLE rank1 #-}

instance Rank1 [Bool] where
  rank1 = go 0
    where go r _ 0          = r
          go r (True :bs) p = go (r + 1) bs (p - 1)
          go r (False:bs) p = go  r      bs (p - 1)
          go _ [] _         = error "Out of range"
  {-# INLINABLE rank1 #-}

instance Rank1 [Word8] where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 [Word16] where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 [Word32] where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 [Word64] where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word8) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word16) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word32) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word64) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word8) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word16) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word32) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word64) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}
