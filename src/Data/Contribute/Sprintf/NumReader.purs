module NumReader where

-- import Prelude (identity, (<>), show)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Symbol as Symbol
import Prim.Boolean (kind Boolean, True, False)
import Data.Int (fromString)
import Data.Maybe (Maybe)

class IsNumChar (str :: Symbol)  (result :: Boolean) | str -> result
instance isNumChar0 :: IsNumChar "0" True
else instance isNumChar1 :: IsNumChar "1" True
else instance isNumChar2 :: IsNumChar "2" True
else instance isNumChar3 :: IsNumChar "3" True
else instance isNumChar4 :: IsNumChar "4" True
else instance isNumChar5 :: IsNumChar "5" True
else instance isNumChar6 :: IsNumChar "6" True
else instance isNumChar7 :: IsNumChar "7" True
else instance isNumChar8 :: IsNumChar "8" True
else instance isNumChar9 :: IsNumChar "9" True
else instance isNumCharOther :: IsNumChar other False

class NumReader (input :: Symbol) (output :: Symbol) (rest :: Symbol) | input -> output rest

instance nilNumReader :: NumReader "" "" ""
else instance consNumReader ::
  (
    Symbol.Cons head tail input
  , IsNumChar head isNumChar -- これを一般的なLexerにすることで、数値以外にもいろいろと応用可能?
  , NumReaderSub isNumChar head tail output rest
  , IsSymbol output
  ) => NumReader input output rest

class NumReaderSub (isNumChar :: Boolean) (head :: Symbol) (tail :: Symbol) (output :: Symbol) (rest :: Symbol) | isNumChar head tail -> output rest

instance oneNumReaderSub :: NumReaderSub True head "" head "" -- 1文字目が数字で、全体が1文字しかない場合
else instance consNumReaderSub ::
  (
    Symbol.Cons head' tail' tail
  , IsNumChar head' isNumChar
  , NumReaderSub isNumChar head' tail' output' rest'
  , Symbol.Cons head output' output''
  ) => NumReaderSub True head tail output'' rest' -- 1文字目が数字で、全体が二文字以上ある場合
else instance endNumReaderSub ::
  (
    Symbol.Cons head tail rest
  ) => NumReaderSub False head tail "" rest -- 1文字目が数字ではない場合


class NumParser (input :: Symbol)
  where
  parseNum :: SProxy input -> Maybe Int

instance numParser ::
  (
    IsSymbol input
  ) => NumParser input
  where
  parseNum _ = 
    fromString (reflectSymbol (SProxy :: SProxy input))
    -- let maybeInt = fromString (reflectSymbol (SProxy :: SProxy input))
    -- in case maybeInt of
    --   Just n -> n
    --   Nothing -> 0

