module HaskellWorks.Data.RankSelect.Base.Internal
    ( select1Broadword
    ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Int.Narrow

select1Broadword :: Word64 -> Word64 -> Word64
select1Broadword _ 0 = 0
select1Broadword v rn =
  -- Do a normal parallel bit count for a 64-bit integer,
  -- but store all intermediate steps.
  let a = (v .&. 0x5555555555555555) + ((v .>.  1) .&. 0x5555555555555555)    in
  let b = (a .&. 0x3333333333333333) + ((a .>.  2) .&. 0x3333333333333333)    in
  let c = (b .&. 0x0f0f0f0f0f0f0f0f) + ((b .>.  4) .&. 0x0f0f0f0f0f0f0f0f)    in
  let d = (c .&. 0x00ff00ff00ff00ff) + ((c .>.  8) .&. 0x00ff00ff00ff00ff)    in
  let e = (d .&. 0x0000ffff0000ffff) + ((d .>. 16) .&. 0x0000ffff0000ffff)    in
  let f = (e .&. 0x00000000ffffffff) + ((e .>. 32) .&. 0x00000000ffffffff)    in
  -- Now do branchless select!
  let r0 = f + 1 - narrow64 rn                                                in
  let s0 = 64 :: Word64                                                       in
  let t0 = (d .>. 32) + (d .>. 48)                                            in
  let s1 = s0 - ((t0 - r0) .&. 256) .>. 3                                     in
  let r1 = r0 - (t0 .&. ((t0 - r0) .>. 8))                                    in
  let t1 =      (d .>. fromIntegral (s1 - 16)) .&. 0xff                       in
  let s2 = s1 - ((t1 - r1) .&. 256) .>. 4                                     in
  let r2 = r1 - (t1 .&. ((t1 - r1) .>. 8))                                    in
  let t2 =      (c .>. fromIntegral (s2 - 8))  .&. 0xf                        in
  let s3 = s2 - ((t2 - r2) .&. 256) .>. 5                                     in
  let r3 = r2 - (t2 .&. ((t2 - r2) .>. 8))                                    in
  let t3 =      (b .>. fromIntegral (s3 - 4))  .&. 0x7                        in
  let s4 = s3 - ((t3 - r3) .&. 256) .>. 6                                     in
  let r4 = r3 - (t3 .&. ((t3 - r3) .>. 8))                                    in
  let t4 =      (a .>. fromIntegral (s4 - 2))  .&. 0x3                        in
  let s5 = s4 - ((t4 - r4) .&. 256) .>. 7                                     in
  let r5 = r4 - (t4 .&. ((t4 - r4) .>. 8))                                    in
  let t5 =      (v .>. fromIntegral (s5 - 1))  .&. 0x1                        in
  let s6 = s5 - ((t5 - r5) .&. 256) .>. 8                                     in
  fromIntegral s6
{-# INLINE select1Broadword #-}
