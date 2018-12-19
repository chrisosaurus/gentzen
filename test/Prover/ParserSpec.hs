module Prover.ParserSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Lexer
import Prover.Data.Theorem
import Prover.Parser
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
                           , Symbol "axiom", Symbol "a"
                       , Symbol "qed"
                       ]
      let expected = Right $ Theorem { name    = "silly-axiom"
                                     , system  = "G3ip"
                                     , sequent = Sequent.Sequent [Sequent.Symbol "a"] [Sequent.Symbol "a"]
                                     , steps   = [ Axiom "a" ]
                                     }
      parse expression `shouldBe` expected

main :: IO ()
main = hspec spec

