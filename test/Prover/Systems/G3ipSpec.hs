module Prover.Systems.G3ipSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Sequent.Data.Sequent
import Prover.Data.Rule
import Prover.Systems.G3ip

spec :: Spec
spec = do
  describe "g3ip rules" $ do
    it "axiom" $ do
      let expression = apply_axiom axiom [(Symbol "a")] (Sequent [Symbol "a"] [Symbol "a"])
      let expected = Right ()
      expression `shouldBe` expected

main :: IO ()
main = hspec spec

