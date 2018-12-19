module System.CheckSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Sequent.Data.Sequent
import System.Check
import System.Data.Rewrite

spec :: Spec
spec = do
  describe "success cases" $ do
    it "empty success" $ do
      let system = Rule { rule_name = "boring rule"
                        , args = []
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = Unit
                        }
      let expected = Right $ ()
      check_rule system `shouldBe` expected

    it "symbol in prop" $ do
      let system = Rule { rule_name = "defined prop symbol"
                        , args = [Symbol "a"]
                        , left_name = "L"
                        , right_name = "R"
                        , props = [In "a" "L"]
                        , body = Unit
                        }
      let expected = Right ()
      check_rule system `shouldBe` expected

    it "axiom" $ do
      let system = Rule { rule_name = "axiom"
                        , args = [Symbol "a"]
                        , left_name = "L"
                        , right_name = "R"
                        , props = [In "a" "L", In "a" "R"]
                        , body = Unit
                        }
      let expected = Right ()
      check_rule system `shouldBe` expected

    it "left_and" $ do
      let lterm = Expr "L" [ Remove (And (Symbol "a") (Symbol "b"))
                           , Add (Symbol "a")
                           , Add (Symbol "b")
                           ]
      let rterm = SExp (Symbol "R")
      let body = Single (Sequ lterm rterm)
      let system = Rule { rule_name = "left_and"
                        , args = [And (Symbol "a") (Symbol "b")]
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = body
                        }
      let expected = Right ()
      check_rule system `shouldBe` expected

    it "right_and" $ do
      let llterm = Expr "L" [Add (Symbol "a")]
      let rlterm = Expr "L" [Add (Symbol "b")]
      let rterm = Expr "R" [Remove (And (Symbol "a") (Symbol "b"))]
      let lsequ = Sequ llterm rterm
      let rsequ = Sequ rlterm rterm
      let body = Pair lsequ rsequ
      let system = Rule { rule_name = "right_and"
                        , args = [And (Symbol "a") (Symbol "b")]
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = body
                        }
      let expected = Right ()
      check_rule system `shouldBe` expected




  describe "check_rule error cases" $ do
    it "repeated argument error 1" $ do
      let system = Rule { rule_name = "repeating argument"
                        , args = [Symbol "a", Symbol "b", Symbol "a"]
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = Unit
                        }
      let expected = Left $ "in rule 'repeating argument': argument 'a' is repeated."
      check_rule system `shouldBe` expected

    it "repeated argument error 2" $ do
      let system = Rule { rule_name = "repeating argument"
                        , args = [And (Symbol "a") (Symbol "a")]
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = Unit
                        }
      let expected = Left $ "in rule 'repeating argument': argument 'a' is repeated."
      check_rule system `shouldBe` expected

    it "repeated argument error 3" $ do
      let system = Rule { rule_name = "repeating argument"
                        , args = [Implies (Symbol "a") (Symbol "b"), Implies (Symbol "b") (Symbol "c")]
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = Unit
                        }
      let expected = Left $ "in rule 'repeating argument': argument 'b' is repeated."
      check_rule system `shouldBe` expected

    it "repeated sequent name error" $ do
      let system = Rule { rule_name = "repeating sequent name"
                        , args = []
                        , left_name = "L"
                        , right_name = "L"
                        , props = []
                        , body = Unit
                        }
      let expected = Left $ "in rule 'repeating sequent name': argument 'L' is repeated."
      check_rule system `shouldBe` expected

    it "non unique sequent name error" $ do
      let system = Rule { rule_name = "non-unique sequent name"
                        , args = [Symbol "a"]
                        , left_name = "L"
                        , right_name = "a"
                        , props = []
                        , body = Unit
                        }
      let expected = Left $ "in rule 'non-unique sequent name': argument 'a' is repeated."
      check_rule system `shouldBe` expected

    it "left undefined symbol in prop" $ do
      let system = Rule { rule_name = "left undefined prop symbol"
                        , args = [Symbol "a"]
                        , left_name = "L"
                        , right_name = "R"
                        , props = [In "b" "L"]
                        , body = Unit
                        }
      let expected = Left $ "in rule 'left undefined prop symbol': symbol 'b' used in rewrite rule but not defined."
      check_rule system `shouldBe` expected

    it "right undefined symbol in prop" $ do
      let system = Rule { rule_name = "right undefined prop symbol"
                        , args = [Symbol "a"]
                        , left_name = "L"
                        , right_name = "R"
                        , props = [In "a" "M"]
                        , body = Unit
                        }
      let expected = Left $ "in rule 'right undefined prop symbol': symbol 'M' used in rewrite rule but not defined."
      check_rule system `shouldBe` expected

    it "undefined symbol in body 1" $ do
      let lterm = SExp (Symbol "e")
      let rterm = Empty
      let body = Single (Sequ lterm rterm)
      let system = Rule { rule_name = "test"
                        , args = [Symbol "a"]
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = body
                        }
      let expected = Left $ "in rule 'test': symbol 'e' used in rewrite rule but not defined."
      check_rule system `shouldBe` expected

    it "undefined symbol in body 2" $ do
      let lterm = Empty
      let rterm = SExp (Symbol "e")
      let body = Single (Sequ lterm rterm)
      let system = Rule { rule_name = "test"
                        , args = [Symbol "a"]
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = body
                        }
      let expected = Left $ "in rule 'test': symbol 'e' used in rewrite rule but not defined."
      check_rule system `shouldBe` expected

    it "undefined symbol in body 3" $ do
      let lterm = Empty
      let rterm = Expr "e" []
      let body = Single (Sequ lterm rterm)
      let system = Rule { rule_name = "test"
                        , args = [Symbol "a"]
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = body
                        }
      let expected = Left $ "in rule 'test': symbol 'e' used in rewrite rule but not defined."
      check_rule system `shouldBe` expected

    it "undefined symbol in body 4" $ do
      let lterm = Empty
      let rterm = Expr "L" [ Add (Symbol "a")
                           , Add (Symbol "b")
                           , Add (Symbol "L")
                           , Add (Symbol "e")
                           , Add (Symbol "R")
                           ]
      let body = Single (Sequ lterm rterm)
      let system = Rule { rule_name = "test"
                        , args = [And (Symbol "a") (Symbol "b")]
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = body
                        }
      let expected = Left $ "in rule 'test': symbol 'e' used in rewrite rule but not defined."
      check_rule system `shouldBe` expected

    it "faulty left_and" $ do
      let lterm = Expr "L" [ Remove (And (Symbol "a") (Symbol "b"))
                           , Add (Symbol "a")
                           , Add (Symbol "b")
                           , Add (Symbol "c")
                           ]
      let rterm = SExp (Symbol "R")
      let body = Single (Sequ lterm rterm)
      let system = Rule { rule_name = "faulty left_and"
                        , args = [And (Symbol "a") (Symbol "b")]
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = body
                        }
      let expected = Left $ "in rule 'faulty left_and': symbol 'c' used in rewrite rule but not defined."
      check_rule system `shouldBe` expected

    it "faulty right_and" $ do
      let llterm = Expr "L" [Add (Symbol "a")]
      let rlterm = Expr "L" [Add (Symbol "c")]
      let rterm = Expr "R" [Remove (And (Symbol "a") (Symbol "b"))]
      let lsequ = Sequ llterm rterm
      let rsequ = Sequ rlterm rterm
      let body = Pair lsequ rsequ
      let system = Rule { rule_name = "faulty right_and"
                        , args = [And (Symbol "a") (Symbol "b")]
                        , left_name = "L"
                        , right_name = "R"
                        , props = []
                        , body = body
                        }
      let expected = Left $ "in rule 'faulty right_and': symbol 'c' used in rewrite rule but not defined."
      check_rule system `shouldBe` expected


main :: IO ()
main = hspec spec

