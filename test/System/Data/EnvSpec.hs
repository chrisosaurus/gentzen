module System.Data.EnvSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Sequent.Data.Sequent
import System.Data.Env

-- theorem silly-axiom
-- system G3ip
-- sequent a |- a
-- proof
--   axiom a
-- qed

spec :: Spec
spec = do
  describe "environment success cases" $ do
    it "exists 1" $ do
      let expression = Env "a" Bottom (Env "b" Bottom EnvEmpty)
      let expected = True
      exists expression "a" `shouldBe` expected
    it "exists 2" $ do
      let expression = Env "a" Bottom (Env "b" Bottom EnvEmpty)
      let expected = True
      exists expression "b" `shouldBe` expected
    it "get 1" $ do
      let expression = Env "a" (Symbol "aval") (Env "b" (Symbol "bval") EnvEmpty)
      let expected = Right (Symbol "aval")
      get expression "a" `shouldBe` expected
    it "get 2" $ do
      let expression = Env "a" (Symbol "aval") (Env "b" (Symbol "bval") EnvEmpty)
      let expected = Right (Symbol "bval")
      get expression "b" `shouldBe` expected
    it "insert" $ do
      let expression = Env "a" (Symbol "aval") (Env "b" (Symbol "bval") EnvEmpty)
      let expected = Right $ Env "q" (Symbol "qval") $ Env "a" (Symbol "aval") $ Env "b" (Symbol "bval") EnvEmpty
      insert expression "q" (Symbol "qval") `shouldBe` expected
    it "remove 1" $ do
      let expression = Env "a" (Symbol "aval") (Env "b" (Symbol "bval") EnvEmpty)
      let expected = Right $ Env "b" (Symbol "bval") EnvEmpty
      remove expression "a" `shouldBe` expected
    it "remove 2" $ do
      let expression = Env "a" (Symbol "aval") (Env "b" (Symbol "bval") EnvEmpty)
      let expected = Right $ Env "a" (Symbol "aval") EnvEmpty
      remove expression "b" `shouldBe` expected


  describe "env error cases" $ do
    it "exists empty" $ do
      let expression = EnvEmpty
      let expected = False
      exists expression "q" `shouldBe` expected
    it "exists 1" $ do
      let expression = Env "a" Bottom (Env "b" Bottom EnvEmpty)
      let expected = False
      exists expression "q" `shouldBe` expected
    it "get" $ do
      let expression = Env "a" Bottom (Env "b" Bottom EnvEmpty)
      let expected = Left "Env.get: environment lookup for 'q' failed."
      get expression "q" `shouldBe` expected
    it "insert duplicate" $ do
      let expression = Env "a" Bottom (Env "b" Bottom EnvEmpty)
      let expected = Left "Env.insert: environment already contained 'b'."
      insert expression "b" Bottom `shouldBe` expected
    it "remove nonexisting" $ do
      let expression = Env "a" Bottom (Env "b" Bottom EnvEmpty)
      let expected = Left "Env.remove: environment did not contain 'q'."
      remove expression "q" `shouldBe` expected


main :: IO ()
main = hspec spec

