module Test.Main where
  
-- import Prelude (Unit, pure, unit,discard)
import Prelude (Unit, discard)
import Effect (Effect)

import Test.Sprintf.Flags (testFlags) as Test.Sprintf.Flags
import Test.Sprintf (testConst, testInt) as Test.Sprintf

main :: Effect Unit
main = do
  Test.Sprintf.Flags.testFlags
  Test.Sprintf.testConst
  Test.Sprintf.testInt