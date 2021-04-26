module Integration.Csound.Opcodes.Logic
  ( logic
  ) where

import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )

import           Data.List                      ( head )
import           Data.Text                      ( Text )


import           Schisma.Csound.Opcodes.Arithmetic
                                                ( Arithmetic((%#), (+#), (-#)) )
import           Schisma.Csound.Opcodes.Logic   ( Comparator
                                                  ( compareEqual
                                                  , compareNotEqual
                                                  )
                                                , CompoundLogicExpression
                                                  ( ifAndE
                                                  , ifOrE
                                                  )
                                                , LogicExpression
                                                  ( ifEqualE
                                                  , ifGreaterThanE
                                                  , ifLessThanE
                                                  )
                                                )
import           Schisma.Csound.Opcodes.Oscillators
                                                ( LowFrequencyOscillator
                                                  ( lfoWithDefaults
                                                  )
                                                , Oscillator
                                                  ( oscil
                                                  , oscilWithDefaults
                                                  , osciliWithDefaults
                                                  )
                                                , vco2
                                                )
import           Schisma.Csound.Opcodes.Phasors ( Phasor(phasorWithDefaults) )
import           Schisma.Csound.Opcodes.Reverberators
                                                ( baboWithDefaults )
import           Schisma.Csound.Opcodes.TableReadWriteOperations
                                                ( tabmorphak )
import           Schisma.Csound.SignalGenerators
                                                ( a#
                                                , i#
                                                , k#
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , KRateSignal
                                                )

import           Integration.Csound.Helpers     ( signalsToInstrumentBlock )

logic :: TestTree
logic = testGroup
  "logic"
  [testIfEqualE, testIfLessThanE, testIfGreaterThanE, testIfAndE, testIfOrE]

testIfEqualE :: TestTree
testIfEqualE =
  let ifEqualEBlock       = signalsToInstrumentBlock [sampleIfEqualE]
      nestedIfEqualEBlock = signalsToInstrumentBlock [sampleNestedIfEqualE]
      multipleSignalsIfEqualEBlock =
        signalsToInstrumentBlock sampleMultipleSignalsIfEqualE
      mixedMultipleSignalsIfEqualEBlock =
        signalsToInstrumentBlock sampleMixedMultipleSignalsIfEqualE
  in  testGroup
        "ifEqualE"
        [ testCase "ifEqualE opcode generation"
        $   ifEqualEBlock
        @?= sampleIfEqualEOutput
        , testCase "ifEqualE opcode generation (nested)"
        $   nestedIfEqualEBlock
        @?= sampleNestedIfEqualEOutput
        , testCase "ifEqualE opcode generation (multiple signals)"
        $   multipleSignalsIfEqualEBlock
        @?= sampleMultipleSignalsIfEqualEOutput
        , testCase "ifEqualE opcode generation (mixed multiple signals)"
        $   mixedMultipleSignalsIfEqualEBlock
        @?= sampleMixedMultipleSignalsIfEqualEOutput
        ]

testIfLessThanE :: TestTree
testIfLessThanE =
  let ifLessThanEBlock = signalsToInstrumentBlock [sampleIfLessThanE]
  in  testGroup
        "ifLessThanE"
        [ testCase "ifLessThanE opcode generation"
          $   ifLessThanEBlock
          @?= sampleIfLessThanEOutput
        ]

testIfGreaterThanE :: TestTree
testIfGreaterThanE =
  let ifGreaterThanEBlock = signalsToInstrumentBlock [sampleIfGreaterThanE]
      nestedIfGreaterThanEBlock =
        signalsToInstrumentBlock [sampleNestedIfGreaterThanE]
  in  testGroup
        "ifGreaterThanE"
        [ testCase "ifGreaterThanE opcode generation"
        $   ifGreaterThanEBlock
        @?= sampleIfGreaterThanEOutput
        , testCase "ifGreaterThanE opcode generation (nested)"
        $   nestedIfGreaterThanEBlock
        @?= sampleNestedIfGreaterThanEOutput
        ]

testIfAndE :: TestTree
testIfAndE =
  let ifAndEBlock        = signalsToInstrumentBlock [sampleIfAndE]
      nestedIfAndEBlock  = signalsToInstrumentBlock [sampleNestedIfAndE]
      chainedIfAndEBlock = signalsToInstrumentBlock [sampleChainedIfAndE]
  in  testGroup
        "ifAndE"
        [ testCase "ifAndE opcode generation"
        $   ifAndEBlock
        @?= sampleIfAndEOutput
        , testCase "ifAndE opcode generation (nested)"
        $   nestedIfAndEBlock
        @?= sampleNestedIfAndEOutput
        , testCase "ifAndE opcode generation (chained)"
        $   chainedIfAndEBlock
        @?= sampleChainedIfAndEOutput
        ]

testIfOrE :: TestTree
testIfOrE =
  let ifOrEBlock        = signalsToInstrumentBlock [sampleIfOrE]
      nestedIfOrEBlock  = signalsToInstrumentBlock [sampleNestedIfOrE]
      chainedIfOrEBlock = signalsToInstrumentBlock [sampleChainedIfOrE]
  in  testGroup
        "ifOrE"
        [ testCase "ifOrE opcode generation" $ ifOrEBlock @?= sampleIfOrEOutput
        , testCase "ifOrE opcode generation (nested)"
        $   nestedIfOrEBlock
        @?= sampleNestedIfOrEOutput
        , testCase "ifOrE opcode generation (chained)"
        $   chainedIfOrEBlock
        @?= sampleChainedIfOrEOutput
        ]


sampleIfEqualE :: ARateSignal
sampleIfEqualE = oscil (a# 0.5) frequency (i# (-1)) (i# 1)
 where
  ifSignal   = oscilWithDefaults (a# 440) (k# 1) :: ARateSignal
  elseSignal = a# 220 :: ARateSignal
  frequency  = ifEqualE (i# 1, i# 2) (ifSignal, elseSignal)

sampleIfEqualEOutput :: [Text]
sampleIfEqualEOutput =
  [ "instr 1"
  , "a1 = 0.5"
  , "i2 = 1.0"
  , "i3 = 2.0"
  , "if (i2 == i3) then"
  , "a4 = 440.0"
  , "k5 = 1.0"
  , "i6 = -1.0"
  , "i7 = 0.0"
  , "a8 oscil a4, k5, i6, i7"
  , "a9 = a8"
  , "else"
  , "a4 = 220.0"
  , "a9 = a4"
  , "endif"
  , "i10 = -1.0"
  , "a11 oscil a1, a9, i10, i2"
  , "out a11"
  , "endin"
  ]

sampleNestedIfEqualE :: ARateSignal
sampleNestedIfEqualE = oscil (a# 0.5) frequency (i# (-1)) (i# 1)
 where
  nestedIfSignal   = a# 110
  nestedThenSignal = a# 880
  nestedFrequency  = ifEqualE (i# 3, i# 4) (nestedIfSignal, nestedThenSignal)
  ifSignal         = oscilWithDefaults nestedFrequency (k# 1) :: ARateSignal
  elseSignal =
    oscilWithDefaults nestedFrequency (k# 2 -# k# 1 :: KRateSignal) :: ARateSignal
  frequency = ifEqualE (i# 1, i# 2) (ifSignal, elseSignal)

sampleNestedIfEqualEOutput :: [Text]
sampleNestedIfEqualEOutput =
  [ "instr 1"
  , "a1 = 0.5"
  , "i2 = 1.0"
  , "i3 = 2.0"
  , "if (i2 == i3) then"
  , "i4 = 3.0"
  , "i5 = 4.0"
  , "if (i4 == i5) then"
  , "a6 = 110.0"
  , "a7 = a6"
  , "else"
  , "a6 = 880.0"
  , "a7 = a6"
  , "endif"
  , "k8 = 1.0"
  , "i9 = -1.0"
  , "i10 = 0.0"
  , "a11 oscil a7, k8, i9, i10"
  , "a14 = a11"
  , "else"
  , "i4 = 3.0"
  , "i5 = 4.0"
  , "if (i4 == i5) then"
  , "a6 = 110.0"
  , "a7 = a6"
  , "else"
  , "a6 = 880.0"
  , "a7 = a6"
  , "endif"
  , "k8 = 2.0"
  , "k9 = 1.0"
  , "k10 = k8 - k9"
  , "i11 = -1.0"
  , "i12 = 0.0"
  , "a13 oscil a7, k10, i11, i12"
  , "a14 = a13"
  , "endif"
  , "i15 = -1.0"
  , "a16 oscil a1, a14, i15, i2"
  , "out a16"
  , "endin"
  ]

sampleMultipleSignalsIfEqualE :: [ARateSignal]
sampleMultipleSignalsIfEqualE = ifEqualE (i# 1, i# 2) (ifSignal, elseSignal)
 where
  source = oscilWithDefaults (a# 0.5) (a# 500)
  ifSignal =
    baboWithDefaults source (k# 6, k# 4, k# 3) (i# 14.39, i# 11.86, i# 10)
  elseSignal = baboWithDefaults source (k# 0, k# 0, k# 0) (i# 1, i# 2, i# 3)

sampleMultipleSignalsIfEqualEOutput :: [Text]
sampleMultipleSignalsIfEqualEOutput =
  [ "instr 1"
  , "i1 = 1.0"
  , "i2 = 2.0"
  , "if (i1 == i2) then"
  , "a3 = 0.5"
  , "a4 = 500.0"
  , "i5 = -1.0"
  , "i6 = 0.0"
  , "a7 oscil a3, a4, i5, i6"
  , "k8 = 6.0"
  , "k9 = 4.0"
  , "k10 = 3.0"
  , "i11 = 14.39"
  , "i12 = 11.86"
  , "i13 = 10.0"
  , "i14 = 8.0"
  , "i15 = -2.0"
  , "i16 = 0.99"
  , "i17 = 0.1"
  , "i18 = 0.3"
  , "i19 = 0.5"
  , "i20 = 0.8"
  , "i21 ftgenonce i6, i6, i14, i15, i16, i17, i6, i6, i6, i18, i19, i20"
  , "a22, a23 babo a7, k8, k9, k10, i11, i12, i13, i1, i21"
  , "a24 = a22"
  , "a25 = a23"
  , "else"
  , "a3 = 0.5"
  , "a4 = 500.0"
  , "i5 = -1.0"
  , "i6 = 0.0"
  , "a7 oscil a3, a4, i5, i6"
  , "k8 = 0.0"
  , "i9 = 3.0"
  , "i10 = 8.0"
  , "i11 = -2.0"
  , "i12 = 0.99"
  , "i13 = 0.1"
  , "i14 = 0.3"
  , "i15 = 0.5"
  , "i16 = 0.8"
  , "i17 ftgenonce i6, i6, i10, i11, i12, i13, i6, i6, i6, i14, i15, i16"
  , "a18, a19 babo a7, k8, k8, k8, i1, i2, i9, i1, i17"
  , "a24 = a18"
  , "a25 = a19"
  , "endif"
  , "out a24, a25"
  , "endin"
  ]

sampleMixedMultipleSignalsIfEqualE :: [ARateSignal]
sampleMixedMultipleSignalsIfEqualE = ifEqualE (i# 1     , i# 2)
                                              (ifSignals, elseSignals)
 where
  oscil1      = oscilWithDefaults (a# 0.5) (k# 400) :: ARateSignal
  oscil2      = osciliWithDefaults (a# 0.25) (k# 200) :: ARateSignal
  oscil3      = lfoWithDefaults (k# 0.75) (k# 20) :: ARateSignal
  oscil4      = lfoWithDefaults (k# 1) (k# 40) :: ARateSignal
  ifSignals   = [oscil1, oscil3]
  elseSignals = baboWithDefaults oscil4 (k# 0, k# 0, k# 0) (i# 1, i# 2, i# 3)

sampleMixedMultipleSignalsIfEqualEOutput :: [Text]
sampleMixedMultipleSignalsIfEqualEOutput =
  [ "instr 1"
  , "i1 = 1.0"
  , "i2 = 2.0"
  , "if (i1 == i2) then"
  , "a3 = 0.5"
  , "k4 = 400.0"
  , "i5 = -1.0"
  , "i6 = 0.0"
  , "a7 oscil a3, k4, i5, i6"
  , "k8 = 0.75"
  , "k9 = 20.0"
  , "a10 lfo k8, k9, i6"
  , "a19 = a7"
  , "a20 = a10"
  , "else"
  , "k3 = 1.0"
  , "k4 = 40.0"
  , "i5 = 0.0"
  , "a6 lfo k3, k4, i5"
  , "k7 = 0.0"
  , "i8 = 3.0"
  , "i9 = 8.0"
  , "i10 = -2.0"
  , "i11 = 0.99"
  , "i12 = 0.1"
  , "i13 = 0.3"
  , "i14 = 0.5"
  , "i15 = 0.8"
  , "i16 ftgenonce i5, i5, i9, i10, i11, i12, i5, i5, i5, i13, i14, i15"
  , "a17, a18 babo a6, k7, k7, k7, i1, i2, i8, i1, i16"
  , "a19 = a17"
  , "a20 = a18"
  , "endif"
  , "out a19, a20"
  , "endin"
  ]

sampleIfGreaterThanE :: ARateSignal
sampleIfGreaterThanE = oscil (a# 0.5) frequency (i# (-1)) (i# 1)
 where
  ifSignal   = oscilWithDefaults (a# 440) (k# 1) :: ARateSignal
  elseSignal = a# 220 :: ARateSignal
  frequency  = ifGreaterThanE (i# 1, i# 2) (ifSignal, elseSignal)

sampleIfGreaterThanEOutput :: [Text]
sampleIfGreaterThanEOutput =
  [ "instr 1"
  , "a1 = 0.5"
  , "i2 = 1.0"
  , "i3 = 2.0"
  , "if (i2 > i3) then"
  , "a4 = 440.0"
  , "k5 = 1.0"
  , "i6 = -1.0"
  , "i7 = 0.0"
  , "a8 oscil a4, k5, i6, i7"
  , "a9 = a8"
  , "else"
  , "a4 = 220.0"
  , "a9 = a4"
  , "endif"
  , "i10 = -1.0"
  , "a11 oscil a1, a9, i10, i2"
  , "out a11"
  , "endin"
  ]

sampleIfLessThanE :: ARateSignal
sampleIfLessThanE = tabmorphak index weight setIndex1 setIndex2 ftns
 where
  position       = k# 0
  index          = phasorWithDefaults (a# 440) :: ARateSignal

  ifLessThanEOne = ifLessThanE (position, k# 1)

  ifWeight       = k# 1 -# position :: KRateSignal
  thenWeight     = k# 2 -# position :: KRateSignal
  weight         = ifLessThanEOne (ifWeight, thenWeight)

  setIndex1      = ifLessThanEOne (k# 0, k# 1)
  setIndex2      = ifLessThanEOne (k# 1, k# 2)

  ftns           = [i# 1, i# 2, i# 3]

sampleIfLessThanEOutput :: [Text]
sampleIfLessThanEOutput =
  [ "instr 1"
  , "a1 = 440.0"
  , "i2 = 0.0"
  , "a3 phasor a1, i2"
  , "k4 = 0.0"
  , "k5 = 1.0"
  , "if (k4 < k5) then"
  , "k6 = k5 - k4"
  , "k8 = k6"
  , "else"
  , "k6 = 2.0"
  , "k7 = k6 - k4"
  , "k8 = k7"
  , "endif"
  , "if (k4 < k5) then"
  , "k9 = k4"
  , "else"
  , "k9 = k5"
  , "endif"
  , "if (k4 < k5) then"
  , "k11 = k5"
  , "else"
  , "k10 = 2.0"
  , "k11 = k10"
  , "endif"
  , "i12 = 1.0"
  , "i13 = 2.0"
  , "i14 = 3.0"
  , "a15 tabmorphak a3, k8, k9, k11, i12, i13, i14"
  , "out a15"
  , "endin"
  ]

sampleNestedIfGreaterThanE :: ARateSignal
sampleNestedIfGreaterThanE = oscil (a# 0.5) frequency (i# (-1)) (i# 1)
 where
  nestedIfSignal   = a# 110
  nestedThenSignal = a# 880
  nestedFrequency =
    ifGreaterThanE (i# 3, i# 4) (nestedIfSignal, nestedThenSignal)
  ifSignal   = oscilWithDefaults nestedFrequency (k# 1) :: ARateSignal
  elseSignal = a# 220 :: ARateSignal
  frequency  = ifGreaterThanE (i# 1, i# 2) (ifSignal, elseSignal)

sampleNestedIfGreaterThanEOutput :: [Text]
sampleNestedIfGreaterThanEOutput =
  [ "instr 1"
  , "a1 = 0.5"
  , "i2 = 1.0"
  , "i3 = 2.0"
  , "if (i2 > i3) then"
  , "i4 = 3.0"
  , "i5 = 4.0"
  , "if (i4 > i5) then"
  , "a6 = 110.0"
  , "a7 = a6"
  , "else"
  , "a6 = 880.0"
  , "a7 = a6"
  , "endif"
  , "k8 = 1.0"
  , "i9 = -1.0"
  , "i10 = 0.0"
  , "a11 oscil a7, k8, i9, i10"
  , "a12 = a11"
  , "else"
  , "a4 = 220.0"
  , "a12 = a4"
  , "endif"
  , "i13 = -1.0"
  , "a14 oscil a1, a12, i13, i2"
  , "out a14"
  , "endin"
  ]

sampleIfAndE :: ARateSignal
sampleIfAndE = oscil (a# 0.5) frequency (i# (-1)) (i# 1)
 where
  iCheck     = compareEqual (i# 1, i# 2)
  kCheck     = compareEqual (k# 3, k# 4)
  ifSignal   = oscilWithDefaults (a# 440) (k# 1) :: ARateSignal
  elseSignal = a# 220 :: ARateSignal
  frequency  = ifAndE (iCheck, kCheck) (ifSignal, elseSignal)

sampleIfAndEOutput :: [Text]
sampleIfAndEOutput =
  [ "instr 1"
  , "a1 = 0.5"
  , "i2 = 1.0"
  , "i3 = 2.0"
  , "k4 = 3.0"
  , "k5 = 4.0"
  , "if (i2 == i3) && (k4 == k5) then"
  , "a6 = 440.0"
  , "k7 = 1.0"
  , "i8 = -1.0"
  , "i9 = 0.0"
  , "a10 oscil a6, k7, i8, i9"
  , "a11 = a10"
  , "else"
  , "a6 = 220.0"
  , "a11 = a6"
  , "endif"
  , "i12 = -1.0"
  , "a13 oscil a1, a11, i12, i2"
  , "out a13"
  , "endin"
  ]

sampleNestedIfAndE :: ARateSignal
sampleNestedIfAndE = oscil (a# 0.5) frequency (i# (-1)) (i# 1)
 where
  nestedKCheck1   = compareEqual (k# 4, k# 5)
  nestedKCheck2   = compareEqual (k# 5, k# 6)
  nestedFrequency = ifAndE (nestedKCheck1, nestedKCheck2) (a# 880, a# 110)

  iCheck          = compareEqual (i# 1, i# 2)
  kCheck          = compareEqual (k# 3, k# 4)

  ifSignal        = oscilWithDefaults nestedFrequency (k# 1) :: ARateSignal
  elseSignal      = a# 220 :: ARateSignal
  frequency       = ifAndE (iCheck, kCheck) (ifSignal, elseSignal)

sampleNestedIfAndEOutput :: [Text]
sampleNestedIfAndEOutput =
  [ "instr 1"
  , "a1 = 0.5"
  , "i2 = 1.0"
  , "i3 = 2.0"
  , "k4 = 3.0"
  , "k5 = 4.0"
  , "if (i2 == i3) && (k4 == k5) then"
  , "k6 = 5.0"
  , "k7 = 6.0"
  , "if (k5 == k6) && (k6 == k7) then"
  , "a8 = 880.0"
  , "a9 = a8"
  , "else"
  , "a8 = 110.0"
  , "a9 = a8"
  , "endif"
  , "k10 = 1.0"
  , "i11 = -1.0"
  , "i12 = 0.0"
  , "a13 oscil a9, k10, i11, i12"
  , "a14 = a13"
  , "else"
  , "a6 = 220.0"
  , "a14 = a6"
  , "endif"
  , "i15 = -1.0"
  , "a16 oscil a1, a14, i15, i2"
  , "out a16"
  , "endin"
  ]

sampleChainedIfAndE :: ARateSignal
sampleChainedIfAndE = signal
 where
  useEnvelope                     = compareNotEqual (k# 1, k# 0)
  modulate                        = compareEqual (k# 10, k# 10)

  defaultPulseWidth               = k# 0.1
  modulatedPulseWidthFromEnvelope = k# 0.3

  pulseWidthFromEnvelope =
    ifAndE
      (useEnvelope, modulate)
      (defaultPulseWidth +# modulatedPulseWidthFromEnvelope, defaultPulseWidth) :: KRateSignal

  useOscillator                     = compareNotEqual (k# 3, k# 4)
  modulatedPulseWidthFromOscillator = k# 0.4

  pulseWidthFromOscillator          = ifAndE
    (useOscillator, modulate)
    ( pulseWidthFromEnvelope +# modulatedPulseWidthFromOscillator
    , pulseWidthFromEnvelope
    )

  pulseWidth = pulseWidthFromOscillator %# k# 1

  signal     = vco2 (k# 1) (k# 440) (i# 18) pulseWidth (k# 0) (i# 0.5)

sampleChainedIfAndEOutput :: [Text]
sampleChainedIfAndEOutput =
  [ "instr 1"
  , "k1 = 1.0"
  , "k2 = 440.0"
  , "i3 = 18.0"
  , "k4 = 3.0"
  , "k5 = 4.0"
  , "k6 = 10.0"
  , "if (k4 != k5) && (k6 == k6) then"
  , "k7 = 0.0"
  , "if (k1 != k7) && (k6 == k6) then"
  , "k8 = 0.1"
  , "k9 = 0.3"
  , "k10 = k8 + k9"
  , "k11 = k10"
  , "else"
  , "k8 = 0.1"
  , "k11 = k8"
  , "endif"
  , "k12 = 0.4"
  , "k13 = k11 + k12"
  , "k14 = k13"
  , "else"
  , "k7 = 0.0"
  , "if (k1 != k7) && (k6 == k6) then"
  , "k8 = 0.1"
  , "k9 = 0.3"
  , "k10 = k8 + k9"
  , "k11 = k10"
  , "else"
  , "k8 = 0.1"
  , "k11 = k8"
  , "endif"
  , "k14 = k11"
  , "endif"
  , "k15 = k14 % k1"
  , "k16 = 0.0"
  , "i17 = 0.5"
  , "a18 vco2 k1, k2, i3, k15, k16, i17"
  , "out a18"
  , "endin"
  ]

sampleIfOrE :: ARateSignal
sampleIfOrE = oscil (a# 0.5) frequency (i# (-1)) (i# 1)
 where
  iCheck     = compareEqual (i# 1, i# 2)
  kCheck     = compareEqual (k# 3, k# 4)
  ifSignal   = oscilWithDefaults (a# 440) (k# 1) :: ARateSignal
  elseSignal = a# 220 :: ARateSignal
  frequency  = ifOrE (iCheck, kCheck) (ifSignal, elseSignal)

sampleIfOrEOutput :: [Text]
sampleIfOrEOutput =
  [ "instr 1"
  , "a1 = 0.5"
  , "i2 = 1.0"
  , "i3 = 2.0"
  , "k4 = 3.0"
  , "k5 = 4.0"
  , "if (i2 == i3) || (k4 == k5) then"
  , "a6 = 440.0"
  , "k7 = 1.0"
  , "i8 = -1.0"
  , "i9 = 0.0"
  , "a10 oscil a6, k7, i8, i9"
  , "a11 = a10"
  , "else"
  , "a6 = 220.0"
  , "a11 = a6"
  , "endif"
  , "i12 = -1.0"
  , "a13 oscil a1, a11, i12, i2"
  , "out a13"
  , "endin"
  ]

sampleNestedIfOrE :: ARateSignal
sampleNestedIfOrE = oscil (a# 0.5) frequency (i# (-1)) (i# 1)
 where
  nestedKCheck1   = compareEqual (k# 4, k# 5)
  nestedKCheck2   = compareEqual (k# 5, k# 6)
  nestedFrequency = ifOrE (nestedKCheck1, nestedKCheck2) (a# 880, a# 110)

  iCheck          = compareEqual (i# 1, i# 2)
  kCheck          = compareEqual (k# 3, k# 4)

  ifSignal        = oscilWithDefaults nestedFrequency (k# 1) :: ARateSignal
  elseSignal      = a# 220 :: ARateSignal
  frequency       = ifOrE (iCheck, kCheck) (ifSignal, elseSignal)

sampleNestedIfOrEOutput :: [Text]
sampleNestedIfOrEOutput =
  [ "instr 1"
  , "a1 = 0.5"
  , "i2 = 1.0"
  , "i3 = 2.0"
  , "k4 = 3.0"
  , "k5 = 4.0"
  , "if (i2 == i3) || (k4 == k5) then"
  , "k6 = 5.0"
  , "k7 = 6.0"
  , "if (k5 == k6) || (k6 == k7) then"
  , "a8 = 880.0"
  , "a9 = a8"
  , "else"
  , "a8 = 110.0"
  , "a9 = a8"
  , "endif"
  , "k10 = 1.0"
  , "i11 = -1.0"
  , "i12 = 0.0"
  , "a13 oscil a9, k10, i11, i12"
  , "a14 = a13"
  , "else"
  , "a6 = 220.0"
  , "a14 = a6"
  , "endif"
  , "i15 = -1.0"
  , "a16 oscil a1, a14, i15, i2"
  , "out a16"
  , "endin"
  ]

sampleChainedIfOrE :: ARateSignal
sampleChainedIfOrE = signal
 where
  useEnvelope        = compareNotEqual (k# 1, k# 0)
  useSecondaryReverb = compareEqual (k# 10, k# 10)

  source             = oscilWithDefaults (a# 0.5) (a# 500)
  defaultReverb =
    baboWithDefaults source (k# 6, k# 4, k# 3) (i# 14.39, i# 11.86, i# 10)
  secondarySource = oscilWithDefaults (a# 0.25) (a# 440)
  secondaryReverb =
    baboWithDefaults secondarySource (k# 0, k# 0, k# 0) (i# 1, i# 2, i# 3)

  defaultPulseWidth               = k# 0.1
  modulatedPulseWidthFromEnvelope = k# 0.3

  reverb =
    ifOrE (useEnvelope, useSecondaryReverb) (secondaryReverb, defaultReverb)
  tertiarySource = oscilWithDefaults (a# 0.75) (a# 880)
  tertiaryReverb =
    baboWithDefaults tertiarySource (k# 0, k# 0, k# 0) (i# 1, i# 2, i# 3)

  useOscillator = compareNotEqual (k# 3, k# 4)
  conditionalReverb =
    ifOrE (useOscillator, useEnvelope) (reverb, tertiaryReverb)

  signal = Data.List.head conditionalReverb

sampleChainedIfOrEOutput :: [Text]
sampleChainedIfOrEOutput =
  [ "instr 1"
  , "k1 = 3.0"
  , "k2 = 4.0"
  , "k3 = 1.0"
  , "k4 = 0.0"
  , "if (k1 != k2) || (k3 != k4) then"
  , "k5 = 10.0"
  , "if (k3 != k4) || (k5 == k5) then"
  , "a6 = 0.25"
  , "a7 = 440.0"
  , "i8 = -1.0"
  , "i9 = 0.0"
  , "a10 oscil a6, a7, i8, i9"
  , "i11 = 1.0"
  , "i12 = 2.0"
  , "i13 = 3.0"
  , "i14 = 8.0"
  , "i15 = -2.0"
  , "i16 = 0.99"
  , "i17 = 0.1"
  , "i18 = 0.3"
  , "i19 = 0.5"
  , "i20 = 0.8"
  , "i21 ftgenonce i9, i9, i14, i15, i16, i17, i9, i9, i9, i18, i19, i20"
  , "a22, a23 babo a10, k4, k4, k4, i11, i12, i13, i11, i21"
  , "a26 = a22"
  , "a27 = a23"
  , "else"
  , "a6 = 0.5"
  , "a7 = 500.0"
  , "i8 = -1.0"
  , "i9 = 0.0"
  , "a10 oscil a6, a7, i8, i9"
  , "k11 = 6.0"
  , "i12 = 14.39"
  , "i13 = 11.86"
  , "i14 = 10.0"
  , "i15 = 1.0"
  , "i16 = 8.0"
  , "i17 = -2.0"
  , "i18 = 0.99"
  , "i19 = 0.1"
  , "i20 = 0.3"
  , "i21 = 0.5"
  , "i22 = 0.8"
  , "i23 ftgenonce i9, i9, i16, i17, i18, i19, i9, i9, i9, i20, i21, i22"
  , "a24, a25 babo a10, k11, k2, k1, i12, i13, i14, i15, i23"
  , "a26 = a24"
  , "a27 = a25"
  , "endif"
  , "a28 = a26"
  , "a29 = a27"
  , "else"
  , "a5 = 0.75"
  , "a6 = 880.0"
  , "i7 = -1.0"
  , "i8 = 0.0"
  , "a9 oscil a5, a6, i7, i8"
  , "i10 = 1.0"
  , "i11 = 2.0"
  , "i12 = 3.0"
  , "i13 = 8.0"
  , "i14 = -2.0"
  , "i15 = 0.99"
  , "i16 = 0.1"
  , "i17 = 0.3"
  , "i18 = 0.5"
  , "i19 = 0.8"
  , "i20 ftgenonce i8, i8, i13, i14, i15, i16, i8, i8, i8, i17, i18, i19"
  , "a21, a22 babo a9, k4, k4, k4, i10, i11, i12, i10, i20"
  , "a28 = a21"
  , "a29 = a22"
  , "endif"
  , "out a28"
  , "endin"
  ]

-- TODO: Test logical condition where if/else bodies are lists of multiple
-- signals (i.e., from different opcodes [so not e.g., babo])
