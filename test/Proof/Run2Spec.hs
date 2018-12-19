module Proof.Run2Spec
(
    main,
    spec,
)
where

import Test.Hspec

import qualified Sequent.Data.Sequent as Sequent
import Proof.Data.Proof
import qualified Proof.Data.Theorem as Theorem
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Data.System
import Proof.Run2

spec :: Spec
spec = do
  describe "step tests" $ do
    it "abort" $ do
      let system   = System { system_name = "boring"
                            , rules       = []
                            }
      let sequent  = Sequent.Sequent [] []
      let proof    = start sequent
      let stmt     = Theorem.Abort
      let expected = Right ([],
                            Proof { sequents = Map.singleton 0 sequent
                                  , steps    = Map.singleton 0 (Abort 0)
                                  , unproven = []
                                  , aborted  = Set.singleton 0
                                  , nextId   = 1
                                  })
      step system proof stmt `shouldBe` expected

    it "print" $ do
      let system   = System { system_name = "boring"
                            , rules       = []
                            }
      let sequent  = Sequent.Sequent [Sequent.Symbol "a"] [Sequent.Symbol "a"]
      let proof    = start sequent
      let stmt     = Theorem.Print
      let expected = Right (["[a] |- [a]"],
                            Proof { sequents = Map.singleton 0 sequent
                                  , steps    = Map.empty
                                  , unproven = [0]
                                  , aborted  = Set.empty
                                  , nextId   = 1
                                  })
      step system proof stmt `shouldBe` expected

    it "expect" $ do
      let system   = System { system_name = "boring"
                            , rules       = []
                            }
      let sequent  = Sequent.Sequent [Sequent.Symbol "a"] [Sequent.Symbol "a"]
      let proof    = start sequent
      let stmt     = Theorem.Expect sequent
      let expected = Right ([],
                            Proof { sequents = Map.singleton 0 sequent
                                  , steps    = Map.empty
                                  , unproven = [0]
                                  , aborted  = Set.empty
                                  , nextId   = 1
                                  })
      step system proof stmt `shouldBe` expected

main :: IO ()
main = hspec spec

