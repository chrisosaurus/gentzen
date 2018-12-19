module System.ParserSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Data.Token
import System.Data.Rewrite
import System.Data.System
import System.Parser
import qualified Sequent.Data.Sequent as Sequent

spec :: Spec
spec = do
  describe "prop parsing" $ do
    it "in" $ do
      let expression = [LParen, Symbol "a", Symbol "in", Symbol "b", RParen]
      let expected = Right $ ([], In (Sequent.Symbol "a") "b")
      parse_prop expression `shouldBe` expected
    it "list of props" $ do
      let expression = [ LSquare
                       , LParen, Symbol "a", Symbol "in", Symbol "b", RParen
                       , Comma
                       , LParen, Symbol "c", Symbol "in", Symbol "d", RParen
                       , RSquare, Period
                       ]
      let expected = Right $ ([], [In (Sequent.Symbol "a") "b", In (Sequent.Symbol "c") "d"])
      parse_props expression `shouldBe` expected

  describe "expr parsing" $ do
    it "plus symbol" $ do
      let expression = [Plus, Symbol "a"]
      let expected = Right $ ([], [Add (Sequent.Symbol "a")])
      parse_exprs expression `shouldBe` expected
    it "minus symbol" $ do
      let expression = [Minus, Symbol "a"]
      let expected = Right $ ([], [Remove (Sequent.Symbol "a")])
      parse_exprs expression `shouldBe` expected
    it "plus sequent" $ do
      let expression = [Plus, LParen, Symbol "a", RParen]
      let expected = Right $ ([], [Add (Sequent.Symbol "a")])
      parse_exprs expression `shouldBe` expected
    it "minus sequent" $ do
      let expression = [Plus, LParen, Symbol "a", And, Symbol "b", RParen]
      let expected = Right $ ([], [Add (Sequent.And (Sequent.Symbol "a") (Sequent.Symbol "b"))])
      parse_exprs expression `shouldBe` expected
    it "no expression empty" $ do
      let expression = []
      let expected = Right $ (expression, [])
      parse_exprs expression `shouldBe` expected
    it "no expression turnstyle" $ do
      let expression = [Turnstyle]
      let expected = Right $ (expression, [])
      parse_exprs expression `shouldBe` expected
    it "no expression garbage" $ do
      let expression = [Symbol "q"]
      let expected = Right $ (expression, [])
      parse_exprs expression `shouldBe` expected

  describe "term parsing" $ do
    it "empty" $ do
      let expression = [LParen, RParen]
      let expected = Right $ ([], Empty)
      parse_term expression `shouldBe` expected
    it "sequent exp" $ do
      let expression = [LParen, Symbol "a", And, Symbol "b", RParen]
      let expected = Right $ ([], SExp (Sequent.And (Sequent.Symbol "a") (Sequent.Symbol "b")))
      parse_term expression `shouldBe` expected
    it "exp" $ do
      let expression = [ Symbol "a"
                       , Minus, Symbol "b"
                       , Plus, LParen, Symbol "c", And, Symbol "d", RParen
                       ]
      let expected = Right $ ([], Expr "a" [ Remove (Sequent.Symbol "b")
                                           , Add (Sequent.And (Sequent.Symbol "c") (Sequent.Symbol "d"))
                                           ])
      parse_term expression `shouldBe` expected

  describe "rewrite rule parsing" $ do
    it "axiom" $ do
      let expression = [ Symbol "axiom"
                       , LParen, Symbol "a", RParen
                       , LParen, Symbol "L", Turnstyle, Symbol "R", RParen
                       , Equal
                       , LSquare
                         , LParen, Symbol "a", Symbol "in", Symbol "L", RParen
                         , Comma
                         , LParen, Symbol "a", Symbol "in", Symbol "R", RParen
                       , RSquare, Period
                       , LParen, RParen
                       ]
      let expected = Right $ ([], Rule { rule_name = "axiom"
                                       , args = [ (Sequent.Symbol "a") ]
                                       , left_name = "L"
                                       , right_name = "R"
                                       , props = [ (In (Sequent.Symbol "a") "L")
                                                 , (In (Sequent.Symbol "a") "R")
                                                 ]
                                       , body = Unit
                                       })
      parse_rule expression `shouldBe` expected
    it "left_and" $ do
      let expression = [ Symbol "left_and"
                       , LParen, Symbol "a", And, Symbol "b", RParen
                       , LParen, Symbol "L", Turnstyle, Symbol "R", RParen
                       , Equal
                       , Symbol "L"
                       , Minus, LParen, Symbol "a", And, Symbol "b", RParen
                       , Plus, Symbol "a"
                       , Plus, Symbol "b"
                       , Turnstyle
                       , Symbol "R"
                       ]
      let lexp = Expr "L" [ Remove (Sequent.And (Sequent.Symbol "a") (Sequent.Symbol "b"))
                          , Add (Sequent.Symbol "a")
                          , Add (Sequent.Symbol "b")
                          ]
      let rexp = Expr "R" []
      let expected = Right $ ([], Rule { rule_name = "left_and"
                                       , args = [ (Sequent.And (Sequent.Symbol "a") (Sequent.Symbol "b")) ]
                                       , left_name = "L"
                                       , right_name = "R"
                                       , props = []
                                       , body = Single (Sequ lexp rexp)
                                       })
      parse_rule expression `shouldBe` expected
    it "right_and" $ do
      let expression = [ Symbol "right_and"
                       , LParen, Symbol "a", And, Symbol "b", RParen
                       , LParen, Symbol "L", Turnstyle, Symbol "R", RParen
                       , Equal
                       , LParen
                         , Symbol "L", Plus, Symbol "a"
                         , Turnstyle
                         , Symbol "R", Minus, LParen, Symbol "a", And, Symbol "b", RParen
                       , RParen, Comma, LParen
                         , Symbol "L", Plus, Symbol "b"
                         , Turnstyle
                         , Symbol "R", Minus, LParen, Symbol "a", And, Symbol "b", RParen
                       , RParen
                       ]
      let lexp = Sequ (Expr "L" [Add (Sequent.Symbol "a")]) (Expr "R" [Remove (Sequent.And (Sequent.Symbol "a") (Sequent.Symbol "b"))])
      let rexp = Sequ (Expr "L" [Add (Sequent.Symbol "b")]) (Expr "R" [Remove (Sequent.And (Sequent.Symbol "a") (Sequent.Symbol "b"))])
      let expected = Right $ ([], Rule { rule_name = "right_and"
                                       , args = [ (Sequent.And (Sequent.Symbol "a") (Sequent.Symbol "b")) ]
                                       , left_name = "L"
                                       , right_name = "R"
                                       , props = []
                                       , body = Pair lexp rexp
                                       })
      parse_rule expression `shouldBe` expected

  describe "system parsing" $ do
    it "axiom system" $ do
      let expression = [ Symbol "system", Symbol "axiom_system"
                       , Symbol "rules"
                       , Symbol "axiom"
                       , LParen, Symbol "a", RParen
                       , LParen, Symbol "L", Turnstyle, Symbol "R", RParen
                       , Equal
                       , LSquare
                         , LParen, Symbol "a", Symbol "in", Symbol "L", RParen
                         , Comma
                         , LParen, Symbol "a", Symbol "in", Symbol "R", RParen
                       , RSquare, Period
                       , LParen, RParen
                       , Symbol "qed"
                       ]
      let exp_rules = [Rule { rule_name = "axiom"
                            , args = [ (Sequent.Symbol "a") ]
                            , left_name = "L"
                            , right_name = "R"
                            , props = [ (In (Sequent.Symbol "a") "L")
                                      , (In (Sequent.Symbol "a") "R")
                                      ]
                            , body = Unit
                            }]
      let expected = Right System { system_name = "axiom_system"
                                  , rules = exp_rules
                                  }
      parse expression `shouldBe` expected


main :: IO ()
main = hspec spec

