{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.RankSelect.Base.Rank
    ( -- * Rank & Select
      Rank(..)
    ) where

import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.RankSelect.Base.Rank0
import           HaskellWorks.Data.RankSelect.Base.Rank1

class Eq a => Rank v a where
  rank :: a -> v -> Count -> Count

instance Rank [Bool] Bool where
  rank a = if a then rank1 else rank0
  {-# INLINABLE rank #-}
