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

  describe "instantiate" $ do
    it "instantiate" $ do
      let sexp = And (Symbol "a") (Implies (Symbol "a") (Symbol "b"))
      let env = Env "a" [Symbol "1"] $ Env "b" [Symbol "2"] EnvEmpty
      let expected = Right $ And (Symbol "1") (Implies (Symbol "1") (Symbol "2"))
      instantiate sexp env `shouldBe` expected

  describe "rewrite_expr" $ do
    it "rewrite_expr Add 1" $ do
      let expr = (Add (Symbol "a"))
      let env = Env "a" [Symbol "1"] EnvEmpty
      let state = []
      let expected = Right $ [Symbol "1"]
      rewrite_expr expr env state `shouldBe` expected
    it "rewrite_expr Add 2" $ do
      let expr = (Add (Symbol "a"))
      let env = Env "a" [Symbol "1"] EnvEmpty
      let state = [Symbol "q"]
      let expected = Right $ [Symbol "1", Symbol "q"]
      rewrite_expr expr env state `shouldBe` expected
    it "rewrite_expr Remove" $ do
      let expr = (Remove (Symbol "a"))
      let env = Env "a" [Symbol "1"] EnvEmpty
      let state = [Symbol "a", Symbol "1", Symbol "q"]
      let expected = Right $ [Symbol "a", Symbol "q"]
      rewrite_expr expr env state `shouldBe` expected

  describe "rewrite_exprs" $ do
    it "rewrite_exprs 1" $ do
      let exprs = [Add (Symbol "a"), Add (Symbol "b"), Remove (Symbol "b"), Add (Symbol "c")]
      let env = Env "a" [Symbol "1"] $ Env "b" [Symbol "2"] $ Env "c" [Symbol "3"] EnvEmpty
      let state = [Symbol "z"]
      let expected = Right $ [Symbol "3", Symbol "1", Symbol "z"]
      rewrite_exprs exprs env state `shouldBe` expected
    it "rewrite_exprs 2" $ do
      let exprs = []
      let env = EnvEmpty
      let state = [Symbol "z"]
      let expected = Right $ [Symbol "z"]
      rewrite_exprs exprs env state `shouldBe` expected

  describe "rewrite_term" $ do
    it "rewrite_term 1" $ do
      let term = Empty
      let env = Env "a" [Symbol "1"] $ Env "b" [Symbol "2"] $ Env "c" [Symbol "3"] EnvEmpty
      let expected = Right $ []
      rewrite_term term env `shouldBe` expected
    it "rewrite_term 2" $ do
      let term = SExp (And (Symbol "a") (Symbol "b"))
      let env = Env "a" [Symbol "1"] $ Env "b" [Symbol "2"] $ Env "c" [Symbol "3"] EnvEmpty
      let expected = Right $ [And (Symbol "1") (Symbol "2")]
      rewrite_term term env `shouldBe` expected
    it "rewrite_term 3" $ do
      let term = Expr "L" [Add (And (Symbol "a") (Symbol "b"))]
      let env = Env "L" [Symbol "1", Symbol "2"] $ Env "a" [Symbol "3"] $ Env "b" [Symbol "4"] EnvEmpty
      let expected = Right $ [And (Symbol "3") (Symbol "4"), Symbol "1", Symbol "2"]
      rewrite_term term env `shouldBe` expected

  describe "rewrite_sequ" $ do
    it "rewrite_sequ 1" $ do
      let lterm = Empty
      let rterm = Empty
      let sequ = Sequ lterm rterm
      let env = Env "a" [Symbol "1"] $ Env "b" [Symbol "2"] $ Env "c" [Symbol "3"] EnvEmpty
      let expected = Right $ Sequent [] []
      rewrite_sequ sequ env `shouldBe` expected
    it "rewrite_sequ 2" $ do
      let lterm = Expr "L" [Remove (And (Symbol "a") (Symbol "b"))]
      let rterm = Expr "R" []
      let sequ = Sequ lterm rterm
      let env = Env "L" [Symbol "1", Symbol "2", And (Symbol "1") (Symbol "2")] $ Env "R" [Symbol "1"] $ Env "a" [Symbol "1"] $ Env "b" [Symbol "2"] EnvEmpty
      let expected = Right $ Sequent [Symbol "1", Symbol "2"] [Symbol "1"]
      rewrite_sequ sequ env `shouldBe` expected

  describe "rewrite_body" $ do
    it "rewrite_body 1" $ do
      let body = Unit
      let env = Env "a" [Symbol "1"] $ Env "b" [Symbol "2"] $ Env "c" [Symbol "3"] EnvEmpty
      let expected = Right $ []
      rewrite_body body env `shouldBe` expected
    it "rewrite_body 2" $ do
      let lterm = Expr "L" [Remove (And (Symbol "a") (Symbol "b"))]
      let rterm = Expr "R" []
      let body = Single (Sequ lterm rterm)
      let env = Env "L" [Symbol "1", Symbol "2", And (Symbol "1") (Symbol "2")] $ Env "R" [Symbol "1"] $ Env "a" [Symbol "1"] $ Env "b" [Symbol "2"] EnvEmpty
      let expected = Right $ [Sequent [Symbol "1", Symbol "2"] [Symbol "1"]]
      rewrite_body body env `shouldBe` expected
    it "rewrite_body 2" $ do
      let lsequ = Sequ (Expr "L" [Remove (And (Symbol "a") (Symbol "b"))]) (Expr "R" [])
      let rsequ = Sequ (SExp (Symbol "a")) (SExp (Symbol "b"))
      let body = Pair lsequ rsequ
      let env = Env "L" [Symbol "1", Symbol "2", And (Symbol "1") (Symbol "2")] $ Env "R" [Symbol "1"] $ Env "a" [Symbol "1"] $ Env "b" [Symbol "2"] EnvEmpty
      let expected = Right $ [ Sequent [Symbol "1", Symbol "2"] [Symbol "1"]
                             , Sequent [Symbol "1"] [Symbol "2"]
                             ]
      rewrite_body body env `shouldBe` expected

  describe "apply" $ do
    it "apply right_and success" $ do
      let lexp = Sequ (Expr "L" [Add (Symbol "a")]) (Expr "R" [Remove (And (Symbol "a") (Symbol "b"))])
      let rexp = Sequ (Expr "L" [Add (Symbol "b")]) (Expr "R" [Remove (And (Symbol "a") (Symbol "b"))])
      let rule = Rule { rule_name = "right_and"
                      , args = [And (Symbol "a") (Symbol "b")]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = Pair lexp rexp
                      }
      let lseq = [Symbol "q"]
      let rseq = [And (Symbol "1") (Symbol "2"), Symbol "z"]
      let seq = Sequent lseq rseq
      let args = [And (Symbol "1") (Symbol "2")]
      let expected = Right $ [ Sequent [Symbol "1", Symbol "q"] [Symbol "z"]
                             , Sequent [Symbol "2", Symbol "q"] [Symbol "z"]
                             ]
      apply seq rule args `shouldBe` expected
    it "apply right_and error" $ do
      let lexp = Sequ (Expr "L" [Add (Symbol "a")]) (Expr "R" [Remove (And (Symbol "a") (Symbol "b"))])
      let rexp = Sequ (Expr "L" [Add (Symbol "b")]) (Expr "R" [Remove (And (Symbol "a") (Symbol "b"))])
      let rule = Rule { rule_name = "right_and"
                      , args = [And (Symbol "a") (Symbol "b")]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = Pair lexp rexp
                      }
      let lseq = [Symbol "q"]
      let rseq = [And (Symbol "1") (Symbol "3"), Symbol "z"]
      let seq = Sequent lseq rseq
      let args = [And (Symbol "1") (Symbol "2")]
      let expected = Left "Failed to find needle: 1^2"
      apply seq rule args `shouldBe` expected


main :: IO ()
main = hspec spec

