module FlagReader where

-- import Prelude (identity, (<>), (==), (||), show)
import IncludesSymbol

class IsFlagChar (str :: Symbol) (result :: Boolean) | str -> result
instance isFlagCharMinus :: IsFlagChar "-" True
else instance isFlagCharPlus :: IsFlagChar "+" True
else instance isFlagCharSpace :: IsFlagChar " " True
else instance isFlagCharZero :: IsFlagChar "0" True
else instance isFlagCharOther :: IsFlagChar other False

class FlagReader (input :: Symbol) (output :: Symbol) (rest :: Symbol) | input -> output rest

instance nilFlagReader :: FlagReader "" "" ""
else instance consFlagReader ::
  (
    Cons head tail input
  , IsFlagChar head isFlagChar
  , FlagReaderSub isFlagChar head tail output rest
  ) => FlagReader input output rest

class FlagReaderSub (isFlagChar :: Boolean) (head :: Symbol) (tail :: Symbol) (output :: Symbol) (rest :: Symbol) | isFlagChar head tail -> output rest

instance oneFlagReaderSub ::
  (
    IsSymbol head
  ) => FlagReaderSub True head "" head "" -- The case 1st letter is flag and the total length is 1
else instance consFlagReaderSub ::
  (
    Cons head' tail' tail
  , IsFlagChar head' isFlagChar
  , FlagReaderSub isFlagChar head' tail' output' rest'
  , Cons head output' output''
  , IsSymbol head
  , IsSymbol head'
  ) => FlagReaderSub True head tail output'' rest' -- The case 1st letter is flag and total legnth is greater than or equals to 2.
else instance endFlagReaderSub ::
  (
    Cons head tail rest
  ) => FlagReaderSub False head tail "" rest -- The case 1st letter is not flag.

class FlagParser (input :: Symbol)
  where 
  parseFlag :: SProxy input -> { flagMinus :: Boolean, flagPlus :: Boolean, flagSpace :: Boolean, flagZero :: Boolean }

instance flagParser ::
  (
    IncludesSymbol "-" input flagMinus
  , IncludesSymbol "+" input flagPlus
  , IncludesSymbol " " input flagSpace
  , IncludesSymbol "0" input flagZero
  , IsBoolean flagMinus
  , IsBoolean flagPlus
  , IsBoolean flagSpace
  , IsBoolean flagZero

) => FlagParser input
  where
  parseFlag _ = {
    flagMinus: (reflectBoolean (BProxy :: BProxy flagMinus)),
    flagPlus:  (reflectBoolean (BProxy :: BProxy flagPlus)),
    flagSpace: (reflectBoolean (BProxy :: BProxy flagSpace)),
    flagZero:  (reflectBoolean (BProxy :: BProxy flagZero))
  }
