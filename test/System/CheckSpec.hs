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
import System.Data.System

spec :: Spec
spec = do
  describe "check_rule success cases" $ do
    it "empty success" $ do
      let rule = Rule { rule_name = "boring rule"
                      , args = []
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = Unit
                      }
      let expected = Right $ ()
      check_rule [] rule `shouldBe` expected

    it "symbol in prop" $ do
      let rule = Rule { rule_name = "defined prop symbol"
                      , args = [Symbol "a"]
                      , left_name = "L"
                      , right_name = "R"
                      , props = [In (Symbol "a") "L"]
                      , body = Unit
                      }
      let expected = Right ()
      check_rule [] rule `shouldBe` expected

    it "axiom" $ do
      let rule = Rule { rule_name = "axiom"
                      , args = [Symbol "a"]
                      , left_name = "L"
                      , right_name = "R"
                      , props = [In (Symbol "a") "L", In (Symbol "a") "R"]
                      , body = Unit
                      }
      let expected = Right ()
      check_rule [] rule `shouldBe` expected

    it "left_and" $ do
      let lterm = Expr "L" [ Remove (And (Symbol "a") (Symbol "b"))
                           , Add (Symbol "a")
                           , Add (Symbol "b")
                           ]
      let rterm = SExp (Symbol "R")
      let body = Single (Sequ lterm rterm)
      let rule = Rule { rule_name = "left_and"
                      , args = [And (Symbol "a") (Symbol "b")]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = body
                      }
      let expected = Right ()
      check_rule [] rule `shouldBe` expected

    it "right_and" $ do
      let llterm = Expr "L" [Add (Symbol "a")]
      let rlterm = Expr "L" [Add (Symbol "b")]
      let rterm = Expr "R" [Remove (And (Symbol "a") (Symbol "b"))]
      let lsequ = Sequ llterm rterm
      let rsequ = Sequ rlterm rterm
      let body = Pair lsequ rsequ
      let rule = Rule { rule_name = "right_and"
                      , args = [And (Symbol "a") (Symbol "b")]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = body
                      }
      let expected = Right ()
      check_rule [] rule `shouldBe` expected




  describe "check_rule error cases" $ do
    it "repeated argument error 1" $ do
      let rule = Rule { rule_name = "repeating argument"
                      , args = [Symbol "a", Symbol "b", Symbol "a"]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = Unit
                      }
      let expected = Left $ "in rule 'repeating argument' arguments: 'a' is repeated."
      check_rule [] rule `shouldBe` expected

    it "repeated argument error 2" $ do
      let rule = Rule { rule_name = "repeating argument"
                      , args = [And (Symbol "a") (Symbol "a")]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = Unit
                      }
      let expected = Left $ "in rule 'repeating argument' arguments: 'a' is repeated."
      check_rule [] rule `shouldBe` expected

    it "repeated argument error 3" $ do
      let rule = Rule { rule_name = "repeating argument"
                      , args = [Implies (Symbol "a") (Symbol "b"), Implies (Symbol "b") (Symbol "c")]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = Unit
                      }
      let expected = Left $ "in rule 'repeating argument' arguments: 'b' is repeated."
      check_rule [] rule `shouldBe` expected

    it "repeated sequent name error" $ do
      let rule = Rule { rule_name = "repeating sequent name"
                      , args = []
                      , left_name = "L"
                      , right_name = "L"
                      , props = []
                      , body = Unit
                      }
      let expected = Left $ "in rule 'repeating sequent name' arguments: 'L' is repeated."
      check_rule [] rule `shouldBe` expected

    it "non unique sequent name error" $ do
      let rule = Rule { rule_name = "non-unique sequent name"
                      , args = [Symbol "a"]
                      , left_name = "L"
                      , right_name = "a"
                      , props = []
                      , body = Unit
                      }
      let expected = Left $ "in rule 'non-unique sequent name' arguments: 'a' is repeated."
      check_rule [] rule `shouldBe` expected

    it "left undefined symbol in prop" $ do
      let rule = Rule { rule_name = "left undefined prop symbol"
                      , args = [Symbol "a"]
                      , left_name = "L"
                      , right_name = "R"
                      , props = [In (Symbol "b") "L"]
                      , body = Unit
                      }
      let expected = Left $ "in rule 'left undefined prop symbol' propositions: 'b' used but not defined."
      check_rule [] rule `shouldBe` expected

    it "right undefined symbol in prop" $ do
      let rule = Rule { rule_name = "right undefined prop symbol"
                      , args = [Symbol "a"]
                      , left_name = "L"
                      , right_name = "R"
                      , props = [In (Symbol "a") "M"]
                      , body = Unit
                      }
      let expected = Left $ "in rule 'right undefined prop symbol' propositions: 'M' used but not defined."
      check_rule [] rule `shouldBe` expected

    it "undefined symbol in body 1" $ do
      let lterm = SExp (Symbol "e")
      let rterm = Empty
      let body = Single (Sequ lterm rterm)
      let rule = Rule { rule_name = "test"
                      , args = [Symbol "a"]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = body
                      }
      let expected = Left $ "in rule 'test' rewrite rule: 'e' used but not defined."
      check_rule [] rule `shouldBe` expected

    it "undefined symbol in body 2" $ do
      let lterm = Empty
      let rterm = SExp (Symbol "e")
      let body = Single (Sequ lterm rterm)
      let rule = Rule { rule_name = "test"
                      , args = [Symbol "a"]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = body
                      }
      let expected = Left $ "in rule 'test' rewrite rule: 'e' used but not defined."
      check_rule [] rule `shouldBe` expected

    it "undefined symbol in body 3" $ do
      let lterm = Empty
      let rterm = Expr "e" []
      let body = Single (Sequ lterm rterm)
      let rule = Rule { rule_name = "test"
                      , args = [Symbol "a"]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = body
                      }
      let expected = Left $ "in rule 'test' rewrite rule: 'e' used but not defined."
      check_rule [] rule `shouldBe` expected

    it "undefined symbol in body 4" $ do
      let lterm = Empty
      let rterm = Expr "L" [ Add (Symbol "a")
                           , Add (Symbol "b")
                           , Add (Symbol "L")
                           , Add (Symbol "e")
                           , Add (Symbol "R")
                           ]
      let body = Single (Sequ lterm rterm)
      let rule = Rule { rule_name = "test"
                      , args = [And (Symbol "a") (Symbol "b")]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = body
                      }
      let expected = Left $ "in rule 'test' rewrite rule: 'e' used but not defined."
      check_rule [] rule `shouldBe` expected

    it "faulty left_and" $ do
      let lterm = Expr "L" [ Remove (And (Symbol "a") (Symbol "b"))
                           , Add (Symbol "a")
                           , Add (Symbol "b")
                           , Add (Symbol "c")
                           ]
      let rterm = SExp (Symbol "R")
      let body = Single (Sequ lterm rterm)
      let rule = Rule { rule_name = "faulty left_and"
                      , args = [And (Symbol "a") (Symbol "b")]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = body
                      }
      let expected = Left $ "in rule 'faulty left_and' rewrite rule: 'c' used but not defined."
      check_rule [] rule `shouldBe` expected

    it "faulty right_and" $ do
      let llterm = Expr "L" [Add (Symbol "a")]
      let rlterm = Expr "L" [Add (Symbol "c")]
      let rterm = Expr "R" [Remove (And (Symbol "a") (Symbol "b"))]
      let lsequ = Sequ llterm rterm
      let rsequ = Sequ rlterm rterm
      let body = Pair lsequ rsequ
      let rule = Rule { rule_name = "faulty right_and"
                      , args = [And (Symbol "a") (Symbol "b")]
                      , left_name = "L"
                      , right_name = "R"
                      , props = []
                      , body = body
                      }
      let expected = Left $ "in rule 'faulty right_and' rewrite rule: 'c' used but not defined."
      check_rule [] rule `shouldBe` expected

  describe "check_system success cases" $ do
    it "empty success" $ do
      let rules = [ Rule { rule_name = "boring rule"
                         , args = []
                         , left_name = "L"
                         , right_name = "R"
                         , props = []
                         , body = Unit
                         }
                  ]
      let system = System { system_name = "boring system"
                          , rules = rules
                          }
      let expected = Right $ ()
      check_system system `shouldBe` expected

    it "axiom system success" $ do
      let rules = [ Rule { rule_name = "axiom"
                         , args = [Symbol "a"]
                         , left_name = "L"
                         , right_name = "R"
                         , props = [In (Symbol "a") "L", In (Symbol "a") "R"]
                         , body = Unit
                         }
                  ]
      let system = System { system_name = "axiom system"
                          , rules = rules
                          }
      let expected = Right $ ()
      check_system system `shouldBe` expected

  describe "check_system failure cases" $ do
    it "clashing rule names" $ do
      let rules = [ Rule { rule_name = "clash"
                         , args = []
                         , left_name = "L"
                         , right_name = "R"
                         , props = []
                         , body = Unit
                         }
                  , Rule { rule_name = "clash"
                         , args = []
                         , left_name = "L"
                         , right_name = "R"
                         , props = []
                         , body = Unit
                         }
                  ]
      let system = System { system_name = "clashing system"
                          , rules = rules
                          }
      let expected = Left "in system 'clashing system': rule 'clash' is repeated."
      check_system system `shouldBe` expected
    it "faulty rule" $ do
      let lterm = Expr "L" [ Remove (And (Symbol "a") (Symbol "b"))
                           , Add (Symbol "a")
                           , Add (Symbol "b")
                           , Add (Symbol "c")
                           ]
      let rules = [ Rule { rule_name = "faulty left_and"
                         , args = [And (Symbol "a") (Symbol "b")]
                         , left_name = "L"
                         , right_name = "R"
                         , props = []
                         , body = Single (Sequ lterm (SExp (Symbol "R")))
                         }
                  ]
      let system = System { system_name = "faulty left_and system"
                          , rules = rules
                          }
      let expected = Left $ "in system 'faulty left_and system': in rule 'faulty left_and' rewrite rule: 'c' used but not defined."
      check_system system `shouldBe` expected


main :: IO ()
main = hspec spec

