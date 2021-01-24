module Integration.Csound.Opcodes.Reverberators
  ( reverberators
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.List
import           Data.Text
import           Data.Text.Prettyprint.Doc


import           Schisma.Csound.SignalGenerators
import           Schisma.Csound.Types

import           Schisma.Csound.Opcodes.Oscillators
import           Schisma.Csound.Opcodes.Reverberators

import           Integration.Csound.Helpers

-- TODO: Fix
-- reverberators = testGroup "reverberators" [testBabo]
reverberators = testGroup "reverberators" []

--testBabo =
--  let baboBlock = signalsToInstrumentBlock sampleBabo
--      baboWithDefaultsBlock = signalsToInstrumentBlock sampleBaboWithDefaults
--  in  testGroup
--        "babo"
--        [ testCase "babo opcode generation"
--            $ instrumentBlockCompare baboBlock sampleBaboOutput
--
--        , testCase "baboWithDefaults opcode generation"
--            $ instrumentBlockCompare baboWithDefaultsBlock sampleBaboWithDefaultsOutput
--
--        ]
--
--
--sampleBabo :: [ARateSignal]
--sampleBabo = babo input (x, y, z) (width, depth, height) diffusion ftn
-- where
--  input     = oscilWithDefaults (a# 0.5) (a# 500.0)
--  x         = k# 6
--  y         = k# 4
--  z         = k# 3
--  width     = i# 14.39
--  depth     = i# 11.86
--  height    = i# 10
--  diffusion = i# 1
--  ftn       = i# (-2)
--
--sampleBaboOutput :: Doc Text
--sampleBaboOutput = linesToInstrumentBlock
--  [ "instr 1"
--  , "a1 = 0.5"
--  , "a2 = 500.0"
--  , "i3 = -1.0"
--  , "i4 = 0.0"
--  , "a5 oscil a1, a2, i3, i4"
--  , "k6 = 6.0"
--  , "k7 = 4.0"
--  , "k8 = 3.0"
--  , "i9 = 14.39"
--  , "i10 = 11.86"
--  , "i11 = 10.0"
--  , "i12 = 1.0"
--  , "i13 = -2.0"
--  , "a14, a15 babo a5, k6, k7, k8, i9, i10, i11, i12, i13"
--  , "out a14, a15"
--  , "endin"
--  ]
--
--
--sampleBaboWithDefaults :: [ARateSignal]
--sampleBaboWithDefaults = baboWithDefaults input (x, y, z) (width, depth, height)
-- where
--  input  = oscilWithDefaults (a# 0.5) (a# 500.0)
--  x      = pk# 4
--  y      = pk# 5
--  z      = pk# 6
--  width  = pi# 7
--  depth  = pi# 8
--  height = pi# 9
--
--sampleBaboWithDefaultsOutput :: Doc Text
--sampleBaboWithDefaultsOutput = linesToInstrumentBlock
--  [ "instr 1"
--  , "a1 = 0.5"
--  , "a2 = 500.0"
--  , "i3 = -1.0"
--  , "i4 = 0.0"
--  , "a5 oscil a1, a2, i3, i4"
--  , "k6 = p4"
--  , "k7 = p5"
--  , "k8 = p6"
--  , "i9 = p7"
--  , "i10 = p8"
--  , "i11 = p9"
--  , "i12 = 1.0"
--  , "i13 = 8.0"
--  , "i14 = -2.0"
--  , "i15 = 0.99"
--  , "i16 = 0.1"
--  , "i17 = 0.3"
--  , "i18 = 0.5"
--  , "i19 = 0.8"
--  , "i20 ftgenonce i4, i4, i13, i14, i15, i16, i4, i4, i4, i17, i18, i19"
--  , "a21, a22 babo a5, k6, k7, k8, i9, i10, i11, i12, i20"
--  , "out a21, a22"
--  , "endin"
--  ]
