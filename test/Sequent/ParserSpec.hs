module Sequent.ParserSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Data.Token
import qualified Sequent.Data.Sequent as Sequent
import Sequent.Parser

spec :: Spec
spec = do
  describe "simple parser tests" $ do
    it "axiom" $ do
      let expression = [Symbol "a", Turnstyle, Symbol "a"]
      let expected = Right $ Sequent.Sequent [Sequent.Symbol "a"] [Sequent.Symbol "a"]
      parse expression `shouldBe` expected
    it "and" $ do
      let expression = [Symbol "a", And, Symbol "b", Turnstyle, Symbol "a"]
      let expected = Right $ Sequent.Sequent [Sequent.And (Sequent.Symbol "a") (Sequent.Symbol "b")] [Sequent.Symbol "a"]
      parse expression `shouldBe` expected
    it "or" $ do
      let expression = [Symbol "a", Or, Symbol "b", Turnstyle, Symbol "a", Comma, Symbol "b"]
      let expected = Right $ Sequent.Sequent [Sequent.Or (Sequent.Symbol "a") (Sequent.Symbol "b")] [Sequent.Symbol "a", Sequent.Symbol "b"]
      parse expression `shouldBe` expected
    it "implies" $ do
      let expression = [Symbol "a", Implies, Symbol "b", Comma, Symbol "a", Turnstyle, Symbol "b"]
      let expected = Right $ Sequent.Sequent [ Sequent.Implies (Sequent.Symbol "a") (Sequent.Symbol "b")
                                             , Sequent.Symbol "a"
                                             ] [Sequent.Symbol "b"]
      parse expression `shouldBe` expected

main :: IO ()
main = hspec spec

