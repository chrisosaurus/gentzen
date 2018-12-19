module System.ApplySpec
(
    main,
    spec,
)
where

import Test.Hspec

import Sequent.Data.Sequent
import System.Apply
import System.Data.Env
import System.Data.Rewrite

spec :: Spec
spec = do
  describe "destruct_sequent" $ do
    it "destruct_sequent" $ do
      let sequent = Sequent [(Symbol "a")] [(Symbol "b")]
      let rule = Rule { rule_name = "boring rule"
                      , args = []
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = Unit
                      }
      let expected = Right $ Env "L" [Symbol "a"] $ Env "R" [Symbol "b"] EnvEmpty
      destruct_sequent sequent rule `shouldBe` expected

  describe "destruct_arg" $ do
    it "destruct_arg and 1" $ do
      let binding  = And (Symbol "a") (Symbol "b")
      let argument = And (Symbol "A") (Symbol "B")
      let expected = Right $ Env "a" [Symbol "A"] $ Env "b" [Symbol "B"] EnvEmpty
      destruct_arg binding argument `shouldBe` expected
    it "destruct_arg and 2" $ do
      let binding  = And (Symbol "a") (Symbol "b")
      let argument = And (And (Symbol "A") (Symbol "B")) (Symbol "C")
      let expected = Right $ Env "a" [And (Symbol "A") (Symbol "B")] $ Env "b" [Symbol "C"] EnvEmpty
      destruct_arg binding argument `shouldBe` expected
    it "destruct_arg and error" $ do
      let binding  = And (Symbol "a") (Symbol "b")
      let argument = Or (Symbol "A") (Symbol "B")
      let expected = Left "Apply error: binding 'a^b' doesn't match argument 'AvB'."
      destruct_arg binding argument `shouldBe` expected
    it "destruct_arg or 1" $ do
      let binding  = Or (Symbol "a") (Symbol "b")
      let argument = Or (Symbol "A") (Symbol "B")
      let expected = Right $ Env "a" [Symbol "A"] $ Env "b" [Symbol "B"] EnvEmpty
      destruct_arg binding argument `shouldBe` expected
    it "destruct_arg or 2" $ do
      let binding  = Or (Symbol "a") (Symbol "b")
      let argument = Or (Implies (Symbol "A") (Symbol "B")) (Symbol "C")
      let expected = Right $ Env "a" [Implies (Symbol "A") (Symbol "B")] $ Env "b" [Symbol "C"] EnvEmpty
      destruct_arg binding argument `shouldBe` expected
    it "destruct_arg implies" $ do
      let binding  = Implies (Symbol "a") (Symbol "b")
      let argument = Implies (Symbol "A") (Symbol "B")
      let expected = Right $ Env "a" [Symbol "A"] $ Env "b" [Symbol "B"] EnvEmpty
      destruct_arg binding argument `shouldBe` expected
    it "destruct_arg bracketed" $ do
      let binding  = Bracketed (Symbol "a")
      let argument = Bracketed (Symbol "A")
      let expected = Right $ Env "a" [Symbol "A"] EnvEmpty
      destruct_arg binding argument `shouldBe` expected
    it "destruct_arg bracketed" $ do
      let binding  = Symbol "a"
      let argument = Symbol "A"
      let expected = Right $ Env "a" [Symbol "A"] EnvEmpty
      destruct_arg binding argument `shouldBe` expected

  describe "destruct_args'" $ do
    it "destruct_args' 1" $ do
      let binding  = [And (Symbol "a") (Symbol "b")]
      let argument = [And (Symbol "A") (Symbol "B")]
      let expected = Right $ Env "a" [Symbol "A"] $ Env "b" [Symbol "B"] EnvEmpty
      destruct_args' binding argument `shouldBe` expected
    it "destruct_args' a" $ do
      let binding  = [And (Symbol "a") (Symbol "b"), (Symbol "c")]
      let argument = [And (Symbol "A") (Symbol "B"), (Symbol "C")]
      let expected = Right $ Env "a" [Symbol "A"] $ Env "b" [Symbol "B"] $ Env "c" [Symbol "C"] EnvEmpty
      destruct_args' binding argument `shouldBe` expected
    it "destruct_args' error 1" $ do
      let binding  = [And (Symbol "a") (Symbol "b")]
      let argument = [Or (Symbol "A") (Symbol "B")]
      let expected = Left "Apply error: binding 'a^b' doesn't match argument 'AvB'."
      destruct_args' binding argument `shouldBe` expected
    it "destruct_args' error 2" $ do
      let binding  = [And (Symbol "a") (Symbol "b"), (Symbol "c")]
      let argument = [And (Symbol "A") (Symbol "B")]
      let expected = Left "Too few arguments."
      destruct_args' binding argument `shouldBe` expected
    it "destruct_args' error 3" $ do
      let binding  = [And (Symbol "a") (Symbol "b")]
      let argument = [And (Symbol "A") (Symbol "B"), (Symbol "C")]
      let expected = Left "Too many arguments."
      destruct_args' binding argument `shouldBe` expected

  describe "destruct_args" $ do
    it "destruct_args 1" $ do
      let rule  = Rule { rule_name="test"
                       , args=[And (Symbol "a") (Symbol "b")]
                       , left_name="L"
                       , right_name="L"
                       , props=[]
                       , body = Unit
                       }
      let argument = [And (Symbol "A") (Symbol "B")]
      let expected = Right $ Env "a" [Symbol "A"] $ Env "b" [Symbol "B"] EnvEmpty
      destruct_args rule argument `shouldBe` expected
    it "destruct_args a" $ do
      let rule  = Rule { rule_name="test"
                       , args=[And (Symbol "a") (Symbol "b"), (Symbol "c")]
                       , left_name="L"
                       , right_name="L"
                       , props=[]
                       , body = Unit
                       }
      let argument = [And (Symbol "A") (Symbol "B"), (Symbol "C")]
      let expected = Right $ Env "a" [Symbol "A"] $ Env "b" [Symbol "B"] $ Env "c" [Symbol "C"] EnvEmpty
      destruct_args rule argument `shouldBe` expected
    it "destruct_args error 1" $ do
      let rule  = Rule { rule_name="test"
                       , args=[And (Symbol "a") (Symbol "b")]
                       , left_name="L"
                       , right_name="L"
                       , props=[]
                       , body = Unit
                       }
      let argument = [Or (Symbol "A") (Symbol "B")]
      let expected = Left "Apply error: binding 'a^b' doesn't match argument 'AvB'."
      destruct_args rule argument `shouldBe` expected
    it "destruct_args error 2" $ do
      let rule  = Rule { rule_name="test"
                       , args=[And (Symbol "a") (Symbol "b"), (Symbol "c")]
                       , left_name="L"
                       , right_name="L"
                       , props=[]
                       , body = Unit
                       }
      let argument = [And (Symbol "A") (Symbol "B")]
      let expected = Left "Argument number mismatch for rule 'test', expected: 2, but got 1."
      destruct_args rule argument `shouldBe` expected
    it "destruct_args'error 3" $ do
      let rule  = Rule { rule_name="test"
                       , args=[And (Symbol "a") (Symbol "b")]
                       , left_name="L"
                       , right_name="L"
                       , props=[]
                       , body = Unit
                       }
      let argument = [And (Symbol "A") (Symbol "B"), (Symbol "C")]
      let expected = Left "Argument number mismatch for rule 'test', expected: 1, but got 2."
      destruct_args rule argument `shouldBe` expected


main :: IO ()
main = hspec spec

