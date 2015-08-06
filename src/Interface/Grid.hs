{- |
  Interfaces having to do with grids.

  Generally properties that apply to whole systems.
-}
module Interface.Grid
(
-- * Per-Unit Normalization
  PerUnit (..)
) where


-- | Things which are normalizable to per unit. This is an essential
-- transformation for almost every type of analysis, because it greatly
-- simplifies certain computations (e.g. transformers can be reduced to lines).
--
-- Obviously, @denormalize . normalize = id@.
class PerUnit a where
  normalize :: a -> a
  denormalize :: a -> a
