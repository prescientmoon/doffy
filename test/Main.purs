module Test.Main where

import Prelude

import Control.Ask (class Ask, ask, askFor, local, provide)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy(..))

foo :: Ask Int => String
foo = show (ask :: Int)

bar :: Ask Int => Ask String => Int
bar = ask

spec :: Spec Unit
spec = describe "Control.Ask" do
  describe "ask & provide" do
    it "should allow providing a value" do
      let context = 100
      let result = provide 100 foo
      result `shouldEqual` "100"
    it "should provide the same value when asked more than once" do
      let context = 100
      let result = provide 100 do
            let a = ask :: Int
            let b = ask :: Int
            a == b
      result `shouldEqual` true
    it "should return the correct value when more than 1 argument is in context" do
      let initial = 82
      let first = provide "bruh" $ provide initial bar 
      let second = provide initial $ provide "hey" bar 

      first `shouldEqual` initial
      second `shouldEqual` initial
  describe "askFor" do
    it "should not require a type annotation where `ask` would" do
      let initial = 82
      let result = provide initial (show $ askFor _Int) 
      result `shouldEqual` "82"
  describe "local" do
    it "should do nothing when passed identity" do
      let initial = 34
      let result = provide initial $ local (identity :: Int -> Int) ask
      result `shouldEqual` initial
    it "should allow mapping over the context" do
      let initial = 34
      let updated = local ((*) 2) ask
      let result = provide initial updated
      result `shouldEqual` 68
    it "shoud allow changing the type of the context" do
      let initial = 29
      let result = provide initial $ local (show :: Int -> _) ask
      result `shouldEqual` "29"

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec

---------- Proxies
_Int :: Proxy Int
_Int = Proxy