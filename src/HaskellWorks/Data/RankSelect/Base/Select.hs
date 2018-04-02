{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.RankSelect.Base.Select
    ( -- * Rank & Select
      Select(..)
    ) where

import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Select0
import HaskellWorks.Data.RankSelect.Base.Select1

class Eq a => Select v a where
  select :: a -> v -> Count -> Count

instance Select [Bool] Bool where
  select a = if a then select1 else select0
  {-# INLINABLE select #-}
