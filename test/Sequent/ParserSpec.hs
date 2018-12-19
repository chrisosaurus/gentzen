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

  describe "less simple parser tests" $ do
    it "brackets" $ do
      let expression = [ Symbol "a", Turnstyle
                       , LParen, LParen, Symbol "a", RParen, RParen
                       ]
      let expected = Right $ Sequent.Sequent [Sequent.Symbol "a"] [Sequent.Bracketed (Sequent.Bracketed (Sequent.Symbol "a"))]
      parse expression `shouldBe` expected
    it "brackets and operators 1" $ do
      let expression = [ Symbol "a", Comma, Symbol "b", Turnstyle
                       , LParen, LParen, Symbol "a", RParen, And, Symbol "b" , RParen
                       ]
      let expected = Right $ Sequent.Sequent [Sequent.Symbol "a", Sequent.Symbol "b"] [Sequent.Bracketed (Sequent.And (Sequent.Bracketed (Sequent.Symbol "a")) (Sequent.Symbol "b"))]
      parse expression `shouldBe` expected
    it "brackets and operators 2" $ do
      let expression = [ Symbol "a", Comma, Symbol "b", Turnstyle
                       , LParen, Symbol "a", And, Symbol "b" , RParen
                       ]
      let expected = Right $ Sequent.Sequent [Sequent.Symbol "a", Sequent.Symbol "b"] [Sequent.Bracketed (Sequent.And (Sequent.Symbol "a") (Sequent.Symbol "b"))]
      parse expression `shouldBe` expected

    it "hypothetical syllogism" $ do
      let expression = [ Turnstyle
                       , LParen
                         , LParen, Symbol "a", Implies, Symbol "b", RParen
                         , And
                         , LParen, Symbol "b", Implies, Symbol "c", RParen
                       , RParen
                       , Implies
                       , LParen, Symbol "a", Implies, Symbol "c", RParen
                       ]
      let rexp = Sequent.Implies
                (Sequent.Bracketed (Sequent.And
                    (Sequent.Bracketed (Sequent.Implies (Sequent.Symbol "a") (Sequent.Symbol "b")))
                    (Sequent.Bracketed (Sequent.Implies (Sequent.Symbol "b") (Sequent.Symbol "c")))))
                (Sequent.Bracketed (Sequent.Implies (Sequent.Symbol "a") (Sequent.Symbol "c")))
      let expected = Right $ Sequent.Sequent [] [rexp]
      parse expression `shouldBe` expected


main :: IO ()
main = hspec spec

