import           Test.Tasty
import           Test.Tasty.SmallCheck         as SC
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.HUnit

import           Integration.Csound.Opcodes.Logic
                                               as Logic
import           Integration.Csound.Opcodes.Oscillators
                                               as Oscillators
import           Integration.Csound.Opcodes.Reverberators
                                               as Reverberators

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [integrationTests]

integrationTests :: TestTree
integrationTests = testGroup "Integration Tests" [opcodeTests]

opcodeTests :: TestTree
opcodeTests = testGroup
  "Opcodes"
  [Oscillators.oscillators, Reverberators.reverberators, Logic.logic]
