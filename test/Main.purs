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
import Test.Unit.Output.Simple (runTest)


main = launchAff $ runTest $ do
  describe "builder" $ do
    it "should consruct correct stanzas" $ do
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
      toString s1 `shouldEqual`
        "<iq xmlns='jabber:client'><foo xmlns='jabber:client'/></iq>"
      toString s2 `shouldEqual`
        ("<iq xmlns='jabber:client'>" <>
           "<foo xmlns='jabber:client'>" <>
             "<bar xmlns='jabber:client'>text</bar>" <>
           "</foo>" <>
           "<baz xmlns='jabber:client'/>" <>
         "</iq>")
  describe "exit" $ do
    it "should exit" $ do
      liftEff $ exit (fromEnum Success)
      assert "failed to exit phantomjs" false
