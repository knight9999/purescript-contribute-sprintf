module Test.Sprintf.Flags where
  
import Prelude (Unit, discard)

import Effect (Effect)
import Effect.Console (log)
import Data.Contribute.Sprintf

import Test.Assert (assertEqual')

testFlags :: Effect Unit
testFlags = do
  log "--- Test Flags ---"
  assertEqual' "Check for Symbol '-'" {
    expected : { flagMinus : true, flagPlus : false, flagSpace : false, flagZero : false },
    actual : parseFlag (SProxy :: SProxy "-") 
  }
  assertEqual' "Check for Symbol '0-'" {
    expected : { flagMinus : true, flagPlus : false, flagSpace : false, flagZero : true },
    actual : parseFlag (SProxy :: SProxy "0-") 
  }
  assertEqual' "Check for Symbol ' -+'" {
    expected : { flagMinus : true, flagPlus : true, flagSpace : true, flagZero : false },
    actual : parseFlag (SProxy :: SProxy " -+") 
  }
  assertEqual' "Check for Symbol '0+'" {
    expected : { flagMinus : false, flagPlus : true, flagSpace : false, flagZero : true },
    actual : parseFlag (SProxy :: SProxy "0+") 
  }
  assertEqual' "Check for Symbol ' +'" {
    expected : { flagMinus : false, flagPlus : true, flagSpace : true, flagZero : false },
    actual : parseFlag (SProxy :: SProxy " +") 
  }

