module ParserUtilsSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Data.Token
import ParserUtils

spec :: Spec
spec = do
  describe "parser utility tests" $ do
    it "consume_symbol success" $ do
      let expression = consume_symbol "Hello" [Symbol "Hello", Symbol "World"]
      let expected = Right [Symbol "World"]
      expression `shouldBe` expected
    it "consume_symbol failure" $ do
      let expression = consume_symbol "Hello" [LParen, Symbol "Hello", Symbol "World", RParen]
      let expected = Left "Symbol not found: \"Hello\""
      expression `shouldBe` expected
    it "consume_token success" $ do
      let expression = consume_token LParen [LParen, RParen]
      let expected = Right [RParen]
      expression `shouldBe` expected
    it "consume_token failure" $ do
      let expression = consume_token LParen []
      let expected = Left "Token not found: ("
      expression `shouldBe` expected
    it "expect_empty success" $ do
      let expression = expect_empty []
      let expected = Right []
      expression `shouldBe` expected
    it "expect_empty failure" $ do
      let expression = expect_empty [Symbol "oops"]
      let expected = Left "Expected empty: [oops]"
      expression `shouldBe` expected
    it "parse_string success" $ do
      let expression = parse_string [Symbol "hello", Symbol "world"]
      let expected = Right ([Symbol "world"], "hello")
      expression `shouldBe` expected
    it "parse_string failure" $ do
      let expression = parse_string [LParen, Symbol "world"]
      let expected = Left "Expected string: [(,world]"
      expression `shouldBe` expected

main :: IO ()
main = hspec spec

