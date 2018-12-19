module System.Data.RuleSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Sequent.Data.Sequent
import System.Data.Rule

-- theorem silly-axiom
-- system G3ip
-- sequent a |- a
-- proof
--   axiom a
-- qed

spec :: Spec
spec = do
  describe "unbracket function tests" $ do
    it "unbracket noop" $ do
      let expression = unbracket (Symbol "a")
      let expected = (Symbol "a")
      expression `shouldBe` expected
    it "unbracket many" $ do
      let expression = unbracket $ Bracketed $ Bracketed $ Bracketed $ Symbol "a"
      let expected = (Symbol "a")
      expression `shouldBe` expected
    it "unbracket rule" $ do
      let expression = unbracket $ Bracketed $ Bracketed $ Implies (Bracketed (Symbol "a")) (Bracketed (Symbol "b"))
      let expected = Implies (Bracketed (Symbol "a")) (Bracketed (Symbol "b"))
      expression `shouldBe` expected

  describe "remove_exp tests" $ do
    it "remove_exp error" $ do
      let expression = remove_exp (Symbol "a") []
      let expected = Left $ "Failed to find needle: a"
      expression `shouldBe` expected
    it "remove_exp success" $ do
      let expression = remove_exp (Symbol "b") [(Symbol "a"), (Symbol "b"), (Symbol "b"), (Symbol "c")]
      let expected = Right [(Symbol "a"), (Symbol "b"), (Symbol "c")]
      expression `shouldBe` expected
    it "remove_exp_lhs" $ do
      let lhs  = [Symbol "a", Symbol "b", Symbol "b", Symbol "c"]
      let lhs' = [Symbol "a", Symbol "b", Symbol "c"]
      let rhs  = [Symbol "a", Symbol "b", Symbol "c", Symbol "d"]
      let expression = remove_exp_lhs (Symbol "b") (Sequent lhs rhs)
      let expected = Right (Sequent lhs' rhs)
      expression `shouldBe` expected
    it "remove_exp_lhs" $ do
      let lhs  = [Symbol "a", Symbol "b", Symbol "c", Symbol "d"]
      let rhs  = [Symbol "a", Symbol "b", Symbol "b", Symbol "c"]
      let rhs' = [Symbol "a", Symbol "b", Symbol "c"]
      let expression = remove_exp_rhs (Symbol "b") (Sequent lhs rhs)
      let expected = Right (Sequent lhs rhs')
      expression `shouldBe` expected

  describe "split_exp tests" $ do
    it "split_exp" $ do
      let expression = split_exp (Implies (Symbol "a") (Symbol "b"))
      let expected = Right (Symbol "a", Symbol "b")
      expression `shouldBe` expected
    it "split_exp_lhs" $ do
      let expression = split_exp_lhs (Implies (Symbol "a") (Symbol "b"))
      let expected = Right (Symbol "a")
      expression `shouldBe` expected
    it "split_exp_rhs" $ do
      let expression = split_exp_rhs (Implies (Symbol "a") (Symbol "b"))
      let expected = Right (Symbol "b")
      expression `shouldBe` expected

main :: IO ()
main = hspec spec

