module Test.Main where
  
-- import Prelude (Unit, pure, unit,discard)
import Prelude (Unit, discard, (<>), show)
import Effect (Effect)
import Effect.Console (log)

import Test.Sprintf.Flags (testFlags) as Test.Sprintf.Flags
import Test.Sprintf (testConst, testInt, testFloat) as Test.Sprintf

import Data.Contribute.Sprintf

-- foreign import data SExtendStr :: Symbol -> Symbol
-- foreign import data FExtendStr :: FList -> FList

-- instance formatF_String :: 
--   (
--     FormatF rest restFunc
--   , IsSymbol flags
--   , IsSymbol minWidth
--   , IsSymbol precision
--   , IsSymbol lengthModifier
--   , FlagParser flags

--   ) => FormatF (FExtendStr (FCons (Specifier "s") (FCons (Flag flags) (FCons (MinWidth minWidth) (FCons (Precision precision) (FCons (LengthModifier lengthModifier) rest) )))) ) (String -> restFunc)
--     where
--     formatF _ str
--       = let 
--         flags' = parseFlag (SProxy :: SProxy flags)
--         minWidth' = parseNum (SProxy :: SProxy minWidth)
--         precision' = parseNum (SProxy :: SProxy precision)
--         lengthModifier' = reflectSymbol (SProxy :: SProxy lengthModifier)
--         in
--         \s -> formatF (FProxy :: FProxy rest) (str <> (show s))

main :: Effect Unit
main = do
  -- log ( sprintf (SProxy :: SProxy "++%s++") "OK")
  Test.Sprintf.Flags.testFlags
  Test.Sprintf.testConst
  Test.Sprintf.testInt
  Test.Sprintf.testFloat