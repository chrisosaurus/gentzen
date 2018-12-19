module System.Systems.G3ipSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Sequent.Data.Sequent
import System.Data.Rule
import System.Systems.G3ip

spec :: Spec
spec = do
  describe "g3ip rules" $ do
    it "axiom" $ do
      let sequent = Sequent [Symbol "a"] [Symbol "a"]
      let expression = apply_axiom axiom [(Symbol "a")] sequent
      let expected = Right ()
      expression `shouldBe` expected

    it "left-and" $ do
      let sequent = Sequent [(And (Symbol "a") (Symbol "b")), Symbol "c"] [Symbol "d"]
      let sequent' = Sequent [Symbol "a", Symbol "b", Symbol "c"] [Symbol "d"]
      let expression = apply_unary left_and [And (Symbol "a") (Symbol "b")] sequent
      let expected = Right sequent'
      expression `shouldBe` expected

    it "right-and" $ do
      let sequent = Sequent [Symbol "q"] [And (Symbol "a") (Symbol "b")]
      let sequent_l = Sequent [Symbol "q"] [Symbol "a"]
      let sequent_r = Sequent [Symbol "q"] [Symbol "b"]
      let expression = apply_binary right_and [And (Symbol "a") (Symbol "b")] sequent
      let expected = Right (sequent_l, sequent_r)
      expression `shouldBe` expected

    it "left-or" $ do
      let sequent = Sequent [Or (Symbol "a") (Symbol "b")] [Symbol "c"]
      let sequent_l = Sequent [Symbol "a"] [Symbol "c"]
      let sequent_r = Sequent [Symbol "b"] [Symbol "c"]
      let expression = apply_binary left_or [Or (Symbol "a") (Symbol "b")] sequent
      let expected = Right (sequent_l, sequent_r)
      expression `shouldBe` expected

    it "right-or1" $ do
      let sequent = Sequent [Symbol "q"] [Or (Symbol "a") (Symbol "b")]
      let sequent' = Sequent [Symbol "q"] [Symbol "a"]
      let expression = apply_unary right_or1 [Or (Symbol "a") (Symbol "b")] sequent
      let expected = Right sequent'
      expression `shouldBe` expected

    it "right-or2" $ do
      let sequent = Sequent [Symbol "q"] [Or (Symbol "a") (Symbol "b")]
      let sequent' = Sequent [Symbol "q"] [Symbol "b"]
      let expression = apply_unary right_or2 [Or (Symbol "a") (Symbol "b")] sequent
      let expected = Right sequent'
      expression `shouldBe` expected

    it "left-implies" $ do
      let sequent = Sequent [Implies (Symbol "a") (Symbol "b")] [Symbol "q"]
      let sequent_l = Sequent [Implies (Symbol "a") (Symbol "b")] [Symbol "a"]
      let sequent_r = Sequent [Symbol "b"] [Symbol "q"]
      let expression = apply_binary left_implies [Implies (Symbol "a") (Symbol "b")] sequent
      let expected = Right (sequent_l, sequent_r)
      expression `shouldBe` expected

    it "right-implies" $ do
      let sequent = Sequent [Symbol "q"] [Implies (Symbol "a") (Symbol "b")]
      let sequent' = Sequent [Symbol "a", Symbol "q"] [Symbol "b"]
      let expression = apply_unary right_implies [Implies (Symbol "a") (Symbol "b")] sequent
      let expected = Right sequent'
      expression `shouldBe` expected

    it "left-bottom" $ do
      let sequent = Sequent [Bottom, Symbol "a"] [Symbol "b"]
      let expression = apply_axiom left_bottom [] sequent
      let expected = Right ()
      expression `shouldBe` expected

main :: IO ()
main = hspec spec

