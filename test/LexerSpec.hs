module LexerSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Lexer
import Data.Token

spec :: Spec
spec = do
  describe "single lexer tests" $ do
    it "parens" $ do
      let expression = "()"
      let expected = Right [LParen, RParen]
      lexer expression `shouldBe` expected
    it "symbol 1" $ do
      let expression = "x"
      let expected = Right [Symbol "x"]
      lexer expression `shouldBe` expected
    it "symbol 2" $ do
      let expression = "hello"
      let expected = Right [Symbol "hello"]
      lexer expression `shouldBe` expected
    it "symbol 3" $ do
      let expression = "     hello     world   "
      let expected = Right [Symbol "hello", Symbol "world"]
      lexer expression `shouldBe` expected
    it "symbol 4" $ do
      let expression = "abc123"
      let expected = Right [Symbol "abc123"]
      lexer expression `shouldBe` expected
    it "symbol underscore" $ do
      let expression = "left_and"
      let expected = Right [Symbol "left_and"]
      lexer expression `shouldBe` expected
    it "all spaces" $ do
      let expression = "     "
      let expected = Right []
      lexer expression `shouldBe` expected
    it "turnstyle" $ do
      let expression = "|-"
      let expected = Right [Turnstyle]
      lexer expression `shouldBe` expected
    it "turnstyle ⊢" $ do
      let expression = "⊢"
      let expected = Right [Turnstyle]
      lexer expression `shouldBe` expected
    it "implies" $ do
      let expression = "->"
      let expected = Right [Implies]
      lexer expression `shouldBe` expected
    it "implies →" $ do
      let expression = "→"
      let expected = Right [Implies]
      lexer expression `shouldBe` expected
    it "and ^" $ do
      let expression = "^"
      let expected = Right [And]
      lexer expression `shouldBe` expected
    it "and &" $ do
      let expression = "&"
      let expected = Right [And]
      lexer expression `shouldBe` expected
    it "and ∧" $ do
      let expression = "∧"
      let expected = Right [And]
      lexer expression `shouldBe` expected
    it "or v" $ do
      let expression = "v"
      let expected = Right [Or]
      lexer expression `shouldBe` expected
    it "or V" $ do
      let expression = "V"
      let expected = Right [Or]
      lexer expression `shouldBe` expected
    it "or |" $ do
      let expression = "|"
      let expected = Right [Or]
      lexer expression `shouldBe` expected
    it "or ∨" $ do
      let expression = "∨"
      let expected = Right [Or]
      lexer expression `shouldBe` expected
    it "forall" $ do
      let expression = "forall"
      let expected = Right [Forall]
      lexer expression `shouldBe` expected
    it "forall ∀" $ do
      let expression = "∀"
      let expected = Right [Forall]
      lexer expression `shouldBe` expected
    it "exists" $ do
      let expression = "exists"
      let expected = Right [Exists]
      lexer expression `shouldBe` expected
    it "exists ∀" $ do
      let expression = "∃"
      let expected = Right [Exists]
      lexer expression `shouldBe` expected
    it "bottom _" $ do
      let expression = "_"
      let expected = Right [Bottom]
      lexer expression `shouldBe` expected
    it "bottom Bottom" $ do
      let expression = "Bottom"
      let expected = Right [Bottom]
      lexer expression `shouldBe` expected
    it "plus" $ do
      let expression = "+"
      let expected = Right [Plus]
      lexer expression `shouldBe` expected
    it "minus" $ do
      let expression = "-"
      let expected = Right [Minus]
      lexer expression `shouldBe` expected
    it "period" $ do
      let expression = "."
      let expected = Right [Period]
      lexer expression `shouldBe` expected
    it "equal" $ do
      let expression = "="
      let expected = Right [Equal]
      lexer expression `shouldBe` expected
    it "less than" $ do
      let expression = "<"
      let expected = Right [LessThan]
      lexer expression `shouldBe` expected
    it "greater than" $ do
      let expression = ">"
      let expected = Right [GreaterThan]
      lexer expression `shouldBe` expected
    it "forward slash" $ do
      let expression = "/"
      let expected = Right [ForwardSlash]
      lexer expression `shouldBe` expected

  describe "compound lexer tests" $ do
    it "simple and 1" $ do
      let expression = "a ^ b"
      let expected = Right [Symbol "a", And, Symbol "b"]
      lexer expression `shouldBe` expected
    it "simple and 2" $ do
      let expression = "a^b"
      let expected = Right [Symbol "a", And, Symbol "b"]
      lexer expression `shouldBe` expected
    it "simple or 1" $ do
      let expression = "a v b"
      let expected = Right [Symbol "a", Or, Symbol "b"]
      lexer expression `shouldBe` expected
    it "simple or 2" $ do
      let expression = "a∨b"
      let expected = Right [Symbol "a", Or, Symbol "b"]
      lexer expression `shouldBe` expected
    it "implication" $ do
      let expression = "a -> b, b -> c |- a -> b"
      let expected = Right [Symbol "a", Implies, Symbol "b", Comma, Symbol "b", Implies, Symbol "c", Turnstyle, Symbol "a", Implies, Symbol "b"]
      lexer expression `shouldBe` expected
    it "substitution syntax" $ do
      let expression = "(A<t/x>)"
      let expected = Right [LParen, Symbol "A", LessThan, Symbol "t", ForwardSlash, Symbol "x", GreaterThan, RParen]
      lexer expression `shouldBe` expected
    it "forall syntax" $ do
      let expression = "(∀x A)"
      let expected = Right [LParen, Forall, Symbol "x", Symbol "A", RParen]
      lexer expression `shouldBe` expected
    it "exists syntax" $ do
      let expression = "(∃x A)"
      let expected = Right [LParen, Exists, Symbol "x", Symbol "A", RParen]
      lexer expression `shouldBe` expected

  describe "lexer error handling" $ do
    it "unknown char" $ do
      let expression = "!"
      let expected = Left "Failed to parse: !"
      lexer expression `shouldBe` expected
    it "spaces" $ do
      let expression = "  !"
      let expected = Left "Failed to parse: !"
      lexer expression `shouldBe` expected
    it "invalid symbol" $ do
      let expression = "he!"
      let expected = Left "Failed to parse: !"
      lexer expression `shouldBe` expected

main :: IO ()
main = hspec spec

