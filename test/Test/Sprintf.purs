module Test.Sprintf where
  
import Prelude (Unit, discard)

import Effect (Effect)
import Effect.Console (log)
import Data.Contribute.Sprintf

import Test.Assert (assertEqual')

testConst :: Effect Unit
testConst = do
  log "--- Test Sprintf Const ---"
  assertEqual' "Empty: Check Sprintf \"\"" {
    expected : "",
    actual : sprintf (SProxy :: SProxy "")
  }
  assertEqual' "Hello World: Check Sprintf \"Hello World\"" {
    expected : "Hello World",
    actual : sprintf (SProxy :: SProxy "Hello World")
  }
  assertEqual' "Escape: Check Sprintf \"%%\"" {
    expected : "%",
    actual : sprintf (SProxy :: SProxy "%%")
  }

testInt :: Effect Unit
testInt = do
  log "--- Test Sprintf Int ---"
  assertEqual' "sprintf \"The value = %d\" 5" {
    expected : "The value = 5",
    actual : sprintf (SProxy :: SProxy "The value = %d") 5
  }
  assertEqual' "%d%%Hello(%4d), 2, 5" {
    expected : "2%Hello(   5)",
    actual : sprintf (SProxy :: SProxy "%d%%Hello(%4d)") 2 5
  }
  assertEqual' "%d%%Hello(%04d), 2, 5" {
    expected : "2%Hello(0005)",
    actual : sprintf (SProxy :: SProxy "%d%%Hello(%04d)") 2 5
  }
  assertEqual' "%d%%Hello(%+4d), 2, 5" {
    expected : "2%Hello(  +5)",
    actual : sprintf (SProxy :: SProxy "%d%%Hello(%+4d)") 2 5
  }

testFloat :: Effect Unit
testFloat = do
  log "--- Test Sprintf Float ---"
  assertEqual' "sptrinf \"The value = %f\" 1.0" {
      expected : "The value = 1.000000",
      actual : sprintf (SProxy :: SProxy "The value = %f") 1.0
  }
  assertEqual' "sptrinf \"The value = %.2f\" 2.1" {
      expected : "The value = 2.10",
      actual : sprintf (SProxy :: SProxy "The value = %.2f") 2.1
  }