module Proof.Data.ProofSpec
(
    main,
    spec,
)
where

import Test.Hspec

import Sequent.Data.Sequent
import Proof.Data.Proof
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "simple Proof usage" $ do
    it "constructor" $ do
      let sequent    = Sequent [] []
      let expression = start sequent
      let expected   = Proof { sequents = Map.singleton 0 sequent
                             , steps    = Map.empty
                             , unproven = [0]
                             , aborted  = Set.empty
                             , nextId   = 1
                             }
      expression `shouldBe` expected

    it "addSeq" $ do
      let sequent    = Sequent [Symbol "second"] []
      let expression = start (Sequent [] [])
      let sequents   = Map.fromList [ (0, Sequent [] [])
                                    , (1, sequent)
                                    ]
      let expected   = Proof { sequents = sequents
                             , steps    = Map.empty
                             , unproven = [1, 0]
                             , aborted  = Set.empty
                             , nextId   = 2
                             }
      addSeq sequent expression `shouldBe` (1, expected)

    it "addStep" $ do
      let step       = Axiom 0 "axiom" []
      let expression = start (Sequent [] [])
      let expected   = Proof { sequents = Map.singleton 0 (Sequent [] [])
                             , steps    = Map.singleton 0 step
                             , unproven = []
                             , aborted  = Set.empty
                             , nextId   = 1
                             }
      addStep step expression `shouldBe` expected

    it "abortSeq" $ do
      let expression = start (Sequent [] [])
      let expected   = Proof { sequents = Map.singleton 0 (Sequent [] [])
                             , steps    = Map.empty
                             , unproven = []
                             , aborted  = Set.singleton 0
                             , nextId   = 1
                             }
      abortSeq 0 expression `shouldBe` expected

    it "finished false" $ do
      let expression = start (Sequent [] [])
      let expected   = False
      finished expression `shouldBe` expected

    it "finished true" $ do
      let expression = abortSeq 0 $ start (Sequent [] [])
      let expected   = True
      finished expression `shouldBe` expected

main :: IO ()
main = hspec spec

