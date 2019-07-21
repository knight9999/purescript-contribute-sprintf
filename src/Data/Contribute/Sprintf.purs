module Data.Contribute.Sprintf (
sprintf
, class Sprintf
, class FormatF
, formatF
, FProxy
, class Parse
, class Match
, class ParseParam
, class MatchParam
, module FlagReader
, class PrecisionReader
, class PrecisionReaderSub
, class LengthModifierReader
, class LengthModifierReaderSub
, module NumReader
, kind FList
, FNil
, FCons
, kind FToken
, Flag
, MinWidth
, Precision
, LengthModifier
, Specifier
, Literal
, module IncludesSymbol
  ) where

import Prelude (identity, negate, not, (+), (-), (<), (<>), show, (&&), (>), (>=))
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Symbol as Prim.Symbol

import NumReader (class IsNumChar, class NumParser, class NumReader, class NumReaderSub, parseNum)
import FlagReader (class FlagParser, class FlagReader, class FlagReaderSub, class IsFlagChar, parseFlag)
import IncludesSymbol (class Append, class Compare, class Cons, class IncludesSymbol, class IsBoolean, 
  class IsSymbol, class OrderingToBoolean, Append, BProxy(..), Compare, Cons, EQ, False, GT, LT, SProxy(..), 
  True, includesSymbol, reflectBoolean, reflectSymbol)

import Data.Number.Format (fixed, toStringWith)
import Data.Maybe
import Data.String as S

class Sprintf (string :: Symbol) func | string -> func where
  sprintf :: SProxy string -> func

instance sprintf1 ::
  ( Parse string format
  , FormatF format func
  ) => Sprintf string func where
  sprintf _ = formatF (FProxy :: FProxy format) ""

withSignFormat :: Boolean -> Int -> String
withSignFormat flagSign i =
  if (flagSign && (i > 0)) 
    then "+" <> show i
    else show i

class FormatF (format :: FList) func | format -> func where
  formatF :: FProxy format -> String -> func

instance formatF_Nil :: FormatF FNil String where
  formatF _ = identity

instance formatF_Int :: 
  (
    FormatF rest restFunc
  , IsSymbol flags
  , IsSymbol minWidth
  , IsSymbol precision
  , IsSymbol lengthModifier
  , FlagParser flags

  ) => FormatF (FCons (Specifier "d") (FCons (Flag flags) (FCons (MinWidth minWidth) (FCons (Precision precision) (FCons (LengthModifier lengthModifier) rest) )))) (Int -> restFunc)
    where
    formatF _ str
      = let 
        flags' = parseFlag (SProxy :: SProxy flags)
        minWidth' = parseNum (SProxy :: SProxy minWidth)
        precision' = parseNum (SProxy :: SProxy precision)
        lengthModifier' = reflectSymbol (SProxy :: SProxy lengthModifier)
        in
        \i -> formatF (FProxy :: FProxy rest) (str <> (showIntWithFormat flags' minWidth' precision' lengthModifier' i))

else instance formatF_Number ::
  (
    FormatF rest restFunc
  , IsSymbol flags
  , IsSymbol minWidth
  , IsSymbol precision
  , IsSymbol lengthModifier
  , FlagParser flags

  ) => FormatF (FCons (Specifier "f") (FCons (Flag flags) (FCons (MinWidth minWidth) (FCons (Precision precision) (FCons (LengthModifier lengthModifier) rest) )))) (Number -> restFunc)
    where
    formatF _ str
      = let 
        flags' = parseFlag (SProxy :: SProxy flags)
        minWidth' = parseNum (SProxy :: SProxy minWidth)
        precision' = parseNum (SProxy :: SProxy precision)
        lengthModifier' = reflectSymbol (SProxy :: SProxy lengthModifier)
        in
        \f -> formatF (FProxy :: FProxy rest) (str <> (showFloatWithFormat flags' minWidth' precision' lengthModifier' f))

else instance formatF_ConsLiteral ::
  (
    IsSymbol literal
  , FormatF rest fun
  ) => FormatF (FCons (Literal literal) rest) fun where
    formatF _ str
      = formatF (FProxy :: FProxy rest) (str <> reflectSymbol (SProxy :: SProxy literal))


mulStr :: Int -> String -> String
mulStr 0 _ = ""
mulStr n s = s <> (mulStr (n-1) s)


showIntWithFormat :: { flagMinus :: Boolean, flagPlus :: Boolean, flagSpace :: Boolean, flagZero :: Boolean } -> Maybe Int -> Maybe Int -> String -> Int -> String
showIntWithFormat flags maybeMinWidth maybePrecision lengthModifier value =
  let 
    sign = if (value >= 0 && flags.flagPlus) 
      then if flags.flagSpace
        then " "
        else "+" 
      else if (value < 0) 
        then "-"
        else ""
    value' = if (value > 0)
      then show value
      else show (-value)
    width = S.length value'
    --
    precision = case maybePrecision of
      Just precision' -> precision'
      Nothing -> 0
    value'' = if (precision > width)
      then (mulStr (precision - width) "0") <> value'
      else value'
    width' = (S.length sign) + (S.length value'')
    --
    fillChar = if flags.flagZero && (not flags.flagMinus)
      then "0"
      else " "
    fill = case maybeMinWidth of 
      Just minWidth -> if (width' < minWidth)
        then (mulStr (minWidth - width') fillChar)
        else ""
      Nothing -> ""
  in if flags.flagMinus
    then
        sign <> value'' <> fill
    else
      if flags.flagZero
        then sign <> fill <> value''
        else fill <> sign <> value''

showFloatWithFormat :: { flagMinus :: Boolean, flagPlus :: Boolean, flagSpace :: Boolean, flagZero :: Boolean } -> Maybe Int -> Maybe Int -> String -> Number -> String
showFloatWithFormat flags maybeMinWidth maybePrecision lengthModifier value =
  let
    sign = if (value >= 0.0 && flags.flagPlus)
      then if flags.flagSpace
        then " "
        else "+"
      else if (value < 0.0)
        then "-"
        else ""
    precision = case maybePrecision of
      Just precision' -> precision'
      Nothing -> 6
    value' = if (value > 0.0)
      then toStringWith (fixed precision) value
      else toStringWith (fixed precision) (-value)
    width = (S.length sign) + (S.length value')
    fillChar = if flags.flagZero && (not flags.flagMinus)
      then "0"
      else " "
    fill = case maybeMinWidth of 
      Just minWidth -> if (width < minWidth)
        then (mulStr (minWidth - width) fillChar)
        else ""
      Nothing -> ""
  in if flags.flagMinus
    then 
      sign <> value' <> fill
    else
      if flags.flagZero
        then sign <> fill <> value'
        else fill <> sign <> value'

class Parse (string :: Symbol) (format :: FList) | string -> format

foreign import kind FList
foreign import data FNil :: FList
foreign import data FCons :: FToken -> FList -> FList

data FProxy (f :: FList) = FProxy

instance nilParse :: Parse "" (FCons (Literal "") FNil)
else instance consParse :: 
  (
    Prim.Symbol.Cons h t string
  ,  Match h t fl
  ) => Parse string fl

class Match (head :: Symbol) (tail :: Symbol) (out :: FList) | head tail -> out

instance nilMatch :: Match "" "" (FCons (Literal "") FNil)
else instance paramMatch ::
  (
    ParseParam input param
  ) => Match "%" input param
else instance consMatch ::
  (
    Parse input (FCons (Literal literal) rest)
  , Prim.Symbol.Cons head literal literal2
  ) => Match head input (FCons (Literal literal2) rest)

class ParseParam (input :: Symbol) (param :: FList) | input -> param

instance nilParseParam :: ParseParam "" (FCons (Literal "%") FNil)
else instance consParseParam :: 
  (
    Prim.Symbol.Cons h t string
  , MatchParam h t fl
  ) => ParseParam string fl

class MatchParam (head :: Symbol) (tail :: Symbol) (out :: FList) | head tail -> out

instance nilMatchParam :: MatchParam "" "" (FCons (Literal "") FNil)
else instance escapeMatchParam ::
  (
    Parse tail (FCons (Literal literal) rest)
  , Prim.Symbol.Cons "%" literal literal'
  ) => MatchParam "%" tail (FCons (Literal literal') rest)
else instance paramMatchParam ::
  (
    Prim.Symbol.Cons head input input2
  , FlagReader input2 flag input3
  , NumReader input3 minWidth input4
  , PrecisionReader input4 precise input5
  , LengthModifierReader input5 lengthModifier input6
  , Prim.Symbol.Cons head2 input7 input6
  , Parse input7 rest
  , IsSymbol flag
  , IsSymbol minWidth 
  , IsSymbol precise
  , IsSymbol lengthModifier
-- TODO: Checking consistency with Specifier and LengthModifier
  ) => MatchParam head input 
  (FCons 
    (Literal "") 
    (FCons 
      (Specifier head2)
      (FCons 
        (Flag flag)
        (FCons 
          (MinWidth minWidth) 
          (FCons
            (Precision precise)
            (FCons
              (LengthModifier lengthModifier)
              rest
            )
          )
        )
      )
    )
  )

class PrecisionReader (input :: Symbol) (output :: Symbol) (rest :: Symbol) | input -> output rest

instance nilPrecisionReader :: PrecisionReader "" "" ""
else instance existsPrecisionReader ::
  (
    Prim.Symbol.Cons head tail input
  , PrecisionReaderSub head tail output rest
  ) => PrecisionReader input output rest

class PrecisionReaderSub (head :: Symbol) (tail :: Symbol) (output :: Symbol) (rest :: Symbol) | head tail -> output rest

instance existsPrecisionReaderSub :: 
  (
    NumReader tail output rest
  ) => PrecisionReaderSub "." tail output rest
else instance noPrecisionReaderSub :: 
  (
    Prim.Symbol.Cons head tail rest
  ) => PrecisionReaderSub head tail "" rest

class LengthModifierReader (input :: Symbol) (output :: Symbol) (rest :: Symbol) | input -> output rest

instance nilLengthModifierReader :: LengthModifierReader "" "" ""
else instance existsLengthModifierReader ::
  (
    Prim.Symbol.Cons head tail input
  , LengthModifierReaderSub head tail output rest
  ) => LengthModifierReader input output rest

class LengthModifierReaderSub (head :: Symbol) (tail :: Symbol) (output :: Symbol) (rest :: Symbol) | head tail -> output rest

instance existsLengthModifierReaderSub :: LengthModifierReaderSub "l" tail "l" tail
else instance noLengthModifierReaderSub :: 
  (
    Prim.Symbol.Cons head tail rest
  ) => LengthModifierReaderSub head tail "" rest

foreign import kind FToken
foreign import data Flag :: Symbol -> FToken
foreign import data MinWidth :: Symbol -> FToken
foreign import data Precision :: Symbol -> FToken
foreign import data LengthModifier :: Symbol -> FToken
foreign import data Specifier :: Symbol -> FToken
foreign import data Literal :: Symbol -> FToken


