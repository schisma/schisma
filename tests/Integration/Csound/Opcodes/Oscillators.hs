module Integration.Csound.Opcodes.Oscillators
  ( oscillators
  ) where

import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

import           Data.Text                      ( Text )

import           Schisma.Csound.Opcodes.Oscillators
                                                ( Oscillator
                                                  ( oscil
                                                  , oscil3
                                                  , oscil3WithDefaults
                                                  , oscilWithDefaults
                                                  , oscili
                                                  , osciliWithDefaults
                                                  )
                                                )
import           Schisma.Csound.SignalGenerators
                                                ( a#
                                                , i#
                                                , k#
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , KRateSignal
                                                )


import           Integration.Csound.Helpers     ( signalsToInstrumentBlock )

oscillators :: TestTree
oscillators = testGroup "oscillators" [testOscil, testOscili, testOscil3]

testOscil :: TestTree
testOscil =
  let oscilBlock = signalsToInstrumentBlock [sampleOscil]
      oscilWithDefaultsBlock =
        signalsToInstrumentBlock [sampleOscilWithDefaults]
  in  testGroup
        "oscil"
        [ testCase "oscil opcode generation" $ oscilBlock @?= sampleOscilOutput
        , testCase "oscilWithDefaults opcode generation"
        $   oscilWithDefaultsBlock
        @?= sampleOscilWithDefaultsOutput
        ]

testOscili =
  let osciliBlock = signalsToInstrumentBlock [sampleOscili]
      osciliWithDefaultsBlock =
        signalsToInstrumentBlock [sampleOsciliWithDefaults]
  in  testGroup
        "oscili"
        [ testCase "oscili opcode generation"
        $   osciliBlock
        @?= sampleOsciliOutput
        , testCase "osciliWithDefaults opcode generation"
        $   osciliWithDefaultsBlock
        @?= sampleOsciliWithDefaultsOutput
        ]

testOscil3 =
  let oscil3Block = signalsToInstrumentBlock [sampleOscil3]
      oscil3WithDefaultsBlock =
        signalsToInstrumentBlock [sampleOscil3WithDefaults]
  in  testGroup
        "oscil3"
        [ testCase "oscil3 opcode generation"
        $   oscil3Block
        @?= sampleOscil3Output
        , testCase "oscil3WithDefaults opcode generation"
        $   oscil3WithDefaultsBlock
        @?= sampleOscil3WithDefaultsOutput
        ]


sampleOscil :: ARateSignal
sampleOscil = oscil (a# 0.5) (a# 440) (i# (-2)) (i# 1)

sampleOscilOutput :: [Text]
sampleOscilOutput =
  [ "instr 1"
  , "a1 = 0.5"
  , "a2 = 440.0"
  , "i3 = -2.0"
  , "i4 = 1.0"
  , "a5 oscil a1, a2, i3, i4"
  , "out a5"
  , "endin"
  ]


sampleOscilWithDefaults :: ARateSignal
sampleOscilWithDefaults = oscilWithDefaults amplitude (k# 440)
  where amplitude = oscilWithDefaults (k# 0.75) (k# 8000.0) :: KRateSignal

sampleOscilWithDefaultsOutput :: [Text]
sampleOscilWithDefaultsOutput =
  [ "instr 1"
  , "k1 = 0.75"
  , "k2 = 8000.0"
  , "i3 = -1.0"
  , "i4 = 0.0"
  , "k5 oscil k1, k2, i3, i4"
  , "k6 = 440.0"
  , "a7 oscil k5, k6, i3, i4"
  , "out a7"
  , "endin"
  ]


sampleOscili :: ARateSignal
sampleOscili = oscili (k# 0.5) (a# 440) (i# (-2)) (i# 1)

sampleOsciliOutput :: [Text]
sampleOsciliOutput =
  [ "instr 1"
  , "k1 = 0.5"
  , "a2 = 440.0"
  , "i3 = -2.0"
  , "i4 = 1.0"
  , "a5 oscili k1, a2, i3, i4"
  , "out a5"
  , "endin"
  ]


sampleOsciliWithDefaults :: ARateSignal
sampleOsciliWithDefaults = osciliWithDefaults amplitude (k# 440)
  where amplitude = osciliWithDefaults (k# 0.75) (k# 8000.0) :: ARateSignal

sampleOsciliWithDefaultsOutput :: [Text]
sampleOsciliWithDefaultsOutput =
  [ "instr 1"
  , "k1 = 0.75"
  , "k2 = 8000.0"
  , "i3 = -1.0"
  , "i4 = 0.0"
  , "a5 oscili k1, k2, i3, i4"
  , "k6 = 440.0"
  , "a7 oscili a5, k6, i3, i4"
  , "out a7"
  , "endin"
  ]


sampleOscil3 :: ARateSignal
sampleOscil3 = oscil3 (a# 0.5) (a# 440) (i# (-2)) (i# 1)

sampleOscil3Output :: [Text]
sampleOscil3Output =
  [ "instr 1"
  , "a1 = 0.5"
  , "a2 = 440.0"
  , "i3 = -2.0"
  , "i4 = 1.0"
  , "a5 oscil3 a1, a2, i3, i4"
  , "out a5"
  , "endin"
  ]


sampleOscil3WithDefaults :: ARateSignal
sampleOscil3WithDefaults = oscil3WithDefaults amplitude frequency
 where
  amplitude = oscil3WithDefaults (k# 0.75) (k# 8000.0) :: KRateSignal
  frequency = oscil3WithDefaults (k# 0.75) (k# 8000.0) :: KRateSignal

sampleOscil3WithDefaultsOutput :: [Text]
sampleOscil3WithDefaultsOutput =
  [ "instr 1"
  , "k1 = 0.75"
  , "k2 = 8000.0"
  , "i3 = -1.0"
  , "i4 = 0.0"
  , "k5 oscil3 k1, k2, i3, i4"
  , "a6 oscil3 k5, k5, i3, i4"
  , "out a6"
  , "endin"
  ]
