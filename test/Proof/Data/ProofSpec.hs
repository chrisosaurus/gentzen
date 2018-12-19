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

    it "addSeq'" $ do
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
      addSeq' sequent expression `shouldBe` expected

    it "addSeqs" $ do
      let sequent1    = Sequent [Symbol "one"] []
      let sequent2    = Sequent [Symbol "two"] []
      let sequent3    = Sequent [Symbol "three"] []
      let sequents    = [sequent1, sequent2, sequent3]
      let expression  = start (Sequent [] [])
      let sequentsMap = Map.fromList [ (0, Sequent [] [])
                                    , (1, sequent1)
                                    , (2, sequent2)
                                    , (3, sequent3)
                                    ]
      let expected   = Proof { sequents = sequentsMap
                             , steps    = Map.empty
                             , unproven = [3, 2, 1, 0]
                             , aborted  = Set.empty
                             , nextId   = 4
                             }
      addSeqs sequents expression `shouldBe` ([1, 2, 3], expected)

    it "addSeqs'" $ do
      let sequent1    = Sequent [Symbol "one"] []
      let sequent2    = Sequent [Symbol "two"] []
      let sequent3    = Sequent [Symbol "three"] []
      let sequents    = [sequent1, sequent2, sequent3]
      let expression  = start (Sequent [] [])
      let sequentsMap = Map.fromList [ (0, Sequent [] [])
                                    , (1, sequent1)
                                    , (2, sequent2)
                                    , (3, sequent3)
                                    ]
      let expected   = Proof { sequents = sequentsMap
                             , steps    = Map.empty
                             , unproven = [3, 2, 1, 0]
                             , aborted  = Set.empty
                             , nextId   = 4
                             }
      addSeqs' sequents expression `shouldBe` expected

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

    it "tip success" $ do
      let expression = start (Sequent [] [])
      let expected   = Right 0
      tip expression `shouldBe` expected

    it "tip failure" $ do
      let expression = abortSeq 0 $ start (Sequent [] [])
      let expected   = Left "Error: wanted '1' but only had '0' unproven sequents."
      tip expression `shouldBe` expected

    it "tipN failure" $ do
      let expression = start (Sequent [] [])
      let expected   = Left "Error: wanted '2' but only had '1' unproven sequents."
      tipN 2 expression `shouldBe` expected

    it "tipN success" $ do
      let sequents   = [ Sequent [Symbol "one"] []
                       , Sequent [Symbol "two"] []
                       , Sequent [Symbol "three"] []
                       ]
      let expression = addSeqs' sequents $ start (Sequent [] [])
      let expected   = Right [0, 1, 2, 3]
      tipN 4 expression `shouldBe` expected

main :: IO ()
main = hspec spec

