module IncludesSymbol (
  class OrderingToBoolean
, class IncludesSymbol
, includesSymbol
, module Prim.Symbol
, module Prim.Ordering
, module Type.Prelude
) where

-- import Prelude (identity, (<>), show)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol, kind Boolean, True, False, class IsBoolean, BProxy(..), reflectBoolean)
import Type.Data.Boolean (class Or)
import Prim.Symbol (class Append, class Compare, class Cons, Append, Compare, Cons)
import Prim.Ordering (EQ, GT, LT, kind Ordering)

class OrderingToBoolean (target :: Ordering) (result :: Boolean) | target -> result

instance eqOrderingToBoolean :: OrderingToBoolean EQ True
else instance otherOrderingToBoolean :: OrderingToBoolean target False

class IncludesSymbol (target :: Symbol) (list :: Symbol) (result :: Boolean) | target list -> result 
  where
  includesSymbol :: SProxy target -> SProxy list -> Boolean

instance noIncludeSymbol :: IncludesSymbol "" list False
  where
  includesSymbol _ _ = true
else instance nilIncludeSymbol :: IncludesSymbol head "" False
  where
  includesSymbol _ _ = false
else instance findIncludesSymbol :: 
  (
    IsSymbol list
  , Cons head tail list
  , Compare target head compared
  , OrderingToBoolean compared headIncluded
  , IncludesSymbol target tail tailIncluded
  , Or headIncluded tailIncluded result
  , IsBoolean result
  ) => IncludesSymbol target list result
  where
  includesSymbol _ _ = reflectBoolean (BProxy :: BProxy result)

-- else instance otherIncludesSymbol :: IncludesSymbol target list False
--   where
--   includesSymbol _ _ = false

