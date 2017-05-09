module Test.Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.ST (runST)
import Data.Enum (fromEnum)
import Data.StrMap (empty)
import ExitCodes (ExitCode(Success))
import PhantomJS.Phantom (PHANTOMJS, exit, injectJs)
import Strophe (build, c, iq, t, toString, up)
import Test.Unit (describe, it)
import Test.Unit.Assert (assert, shouldEqual)
import Test.Unit.Output.Fancy (runTest)


main = launchAff $ runTest $ do
  describe "builder" $ do
    it "should exit" $ do
      -- _ ← liftEff $ injectJs "/home/paluh/programming/purescript/projects/purescript-strophejs/bower_components/strophejs/strophe.min.js"
      {s1, s2} ← liftEff $ runST (do
        b ← iq empty
        c b "foo" empty
        s1 ← build b
        c b "bar" empty
        t b "text"
        up b
        up b
        c b "baz" empty
        s2 ← build b
        pure {s1, s2})
      toString s1 `shouldEqual` "unknown"
      toString s2 `shouldEqual` "unknown"
