module Proof.ParserSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Lexer
import Proof.Data.Theorem
import Proof.Parser
import qualified Sequent.Data.Sequent as Sequent

-- theorem silly-axiom
-- system G3ip
-- sequent a |- a
-- proof
--   axiom a
-- qed

spec :: Spec
spec = do
  describe "simple parser tests" $ do
    it "axiom" $ do
      let expression = [ Symbol "theorem", Symbol "silly-axiom"
                       , Symbol "system",  Symbol "G3ip"
                       , Symbol "sequent"
                           , Symbol "a", Turnstyle, Symbol "a"
                       , Symbol "proof"
                           , Symbol "apply", Symbol "axiom", LSquare, Symbol "a", RSquare
                       , Symbol "qed"
                       ]
      let expected = Right $ Theorem { name    = "silly-axiom"
                                     , system  = "G3ip"
                                     , sequent = Sequent.Sequent [Sequent.Symbol "a"] [Sequent.Symbol "a"]
                                     , steps   = [ Apply "axiom" [Sequent.Symbol "a"] ]
                                     }
      parse expression `shouldBe` expected

    it "expect" $ do
      let expression = [ Symbol "theorem", Symbol "silly-axiom"
                       , Symbol "system",  Symbol "G3ip"
                       , Symbol "sequent"
                           , Symbol "a", Turnstyle, Symbol "a"
                       , Symbol "proof"
                           , Symbol "expect", Symbol "a", Turnstyle, Symbol "a"
                       , Symbol "qed"
                       ]
      let expected = Right $ Theorem { name    = "silly-axiom"
                                     , system  = "G3ip"
                                     , sequent = Sequent.Sequent [Sequent.Symbol "a"] [Sequent.Symbol "a"]
                                     , steps   = [ Expect (Sequent.Sequent [Sequent.Symbol "a"] [Sequent.Symbol "a"]) ]
                                     }
      parse expression `shouldBe` expected

    it "all commands" $ do
      let expression = [ Symbol "theorem", Symbol "silly-axiom"
                       , Symbol "system",  Symbol "G3ip"
                       , Symbol "sequent"
                           , Symbol "a", Turnstyle, Symbol "a"
                       , Symbol "proof"
                           , Symbol "print"
                           , Symbol "expect", Symbol "a", Turnstyle, Symbol "a"
                           , Symbol "apply", Symbol "axiom",  LSquare, Symbol "a", RSquare
                           , LCurly, Symbol "print", Symbol "abort", RCurly
                           , LCurly, Symbol "abort", RCurly
                       , Symbol "qed"
                       ]
      let expected = Right $ Theorem { name    = "silly-axiom"
                                     , system  = "G3ip"
                                     , sequent = Sequent.Sequent [Sequent.Symbol "a"] [Sequent.Symbol "a"]
                                     , steps   = [ Print
                                                 , Expect (Sequent.Sequent [Sequent.Symbol "a"] [Sequent.Symbol "a"])
                                                 , Apply "axiom" [Sequent.Symbol "a"]
                                                 , Branch [Print, Abort] [Abort]
                                                 ]
                                     }
      parse expression `shouldBe` expected

main :: IO ()
main = hspec spec

