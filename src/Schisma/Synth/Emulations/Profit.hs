module Schisma.Synth.Emulations.Profit
  ( midiSettings
  , profit
  , synthFields
  , synthParameters
  ) where

import           Prelude                 hiding ( init )

import           Data.List                      ( sort )
import           Data.Map.Strict                ( (!)
                                                , Map
                                                , elems
                                                , fromList
                                                , keys
                                                )
import           Data.Text                      ( Text )

import           Schisma.Csound.Opcodes         ( (*#)
                                                , (+#)
                                                , (-#)
                                                , (/#)
                                                , (^#)
                                                , abs#
                                                , adsrForTiedNotes
                                                , ctrl7WithDefaults
                                                , ifEqualE
                                                , ifLessThanE
                                                , init
                                                , int#
                                                , k
                                                , limit
                                                , linseg
                                                , linsegr
                                                , log#
                                                , madsrWithDefaults
                                                , midiAmplitudeAndFrequency
                                                , noise
                                                , pinker
                                                , scaleWithDefaults
                                                , syncphasor
                                                , tival
                                                , vco2
                                                , xin
                                                , xout
                                                , zdfLadder
                                                )
import           Schisma.Csound.SignalGenerators
                                                ( a#
                                                , i#
                                                , k#
                                                , makeRatedSignal
                                                , pi#
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal(..)
                                                , IRateSignal
                                                , IsSignal(getSignal)
                                                , KRateSignal
                                                , Opcode
                                                  ( TerminalOpcode
                                                  , UserDefinedOpcode
                                                  )
                                                , OrdinaryStatement(Op)
                                                , Signal(Signal)
                                                , SignalRate(ARate, IRate)
                                                , Udo(..)
                                                )

import           Schisma.Synth.Types            ( SynthParameter
                                                  ( SynthParameter
                                                  , synthParameterName
                                                  )
                                                )
import           Schisma.Synth.Utilities        ( detune
                                                , frequencyIntervalToSemitones
                                                , mix
                                                )

import           Schisma.Csound.Utilities       ( extratime )

import           Schisma.Utilities              ( merge )

-- TODO: Doc

adsrEnvelope
  :: IRateSignal -> IRateSignal -> IRateSignal -> IRateSignal -> KRateSignal
adsrEnvelope attack decay sustain release = envelope where
  scaledAttack  = scaleEnvelopeKnob attack
  scaledDecay   = scaleEnvelopeKnob decay
  scaledRelease = scaleEnvelopeKnob release

  envelope = adsrForTiedNotes scaledAttack scaledDecay sustain scaledRelease

amplifierEnvelope :: Map Text IRateSignal -> KRateSignal
amplifierEnvelope settings = adsrEnvelope attack decay sustain release where
  attack  = settings ! "amplifierAttack"
  decay   = settings ! "amplifierDecay"
  sustain = settings ! "amplifierSustain"
  release = settings ! "amplifierRelease"

calculateFrequency :: KRateSignal -> KRateSignal -> KRateSignal -> KRateSignal
calculateFrequency frequency semitoneAdjustment fineTuningCentsAdjustment =
  detune frequency totalCentsAdjustment
 where
  centsAdjustment      = semitoneAdjustment *# k# 100
  totalCentsAdjustment = centsAdjustment +# fineTuningCentsAdjustment

defaultSettings :: Map Text IRateSignal
defaultSettings = fromList settings where
  toTuple (SynthParameter name _ _ _ _ defaultValue _) =
    (name, i# defaultValue)
  settings = map toTuple synthParameters

e :: Double
e = 2.7182818284590452354

filterEnvelope :: Map Text IRateSignal -> KRateSignal
filterEnvelope settings = adsrEnvelope attack decay sustain release where
  attack  = settings ! "filterAttack"
  decay   = settings ! "filterDecay"
  sustain = settings ! "filterSustain"
  release = settings ! "filterRelease"

filterInput
  :: Map Text IRateSignal -> ARateSignal -> ARateSignal -> ARateSignal
filterInput settings input oscB = filtered where
  frequency           = settings ! "frequency"

  cfSetting           = k (settings ! "filterCutoffFrequency")

  envelope            = filterEnvelope settings
  envelopeAmount      = k (settings ! "filterEnvelopeAmount")

  keyboardAmount      = k (settings ! "filterKeyboardAmount")

  defaultCfLowerBound = k# 8.3
  defaultCfUpperBound = k# 8499.25

  defaultCutoffFrequency =
    scaleCutoffFrequency defaultCfLowerBound defaultCfUpperBound cfSetting

  keyboardCfLowerBound = i# 32.7
  semitonesBetween =
    frequencyIntervalToSemitones keyboardCfLowerBound frequency
  keyboardCfUpperBound =
    calculateFrequency (k frequency) (k semitonesBetween) (k# 0)

  scaledKeyboardCutoffFrequency = scaleCutoffFrequency
    (k keyboardCfLowerBound)
    keyboardCfUpperBound
    keyboardAmount

  keyboardCutoffFrequency =
    ifEqualE (keyboardAmount, k# 0) (k# 0, scaledKeyboardCutoffFrequency)

  cutoffFrequencyWithEnvelope =
    envelope
      *# scaleCutoffFrequency defaultCutoffFrequency (k# 22050) envelopeAmount

  centerCutoffFrequency = ifEqualE
    (envelopeAmount, k# 0)
    ( defaultCutoffFrequency
    , cutoffFrequencyWithEnvelope +# defaultCutoffFrequency
    )

  modulatedCutoffFrequencyFromPolyModFilterEnvelope =
    k (settings ! "polyModSourceAmountFilterEnvelope") *# envelope

  modulatedCutoffFrequencyFromOscillatorB =
    k (settings ! "polyModSourceAmountOscillatorB") *# k oscB

  modulatedCutoffFrequency =
    (  modulatedCutoffFrequencyFromPolyModFilterEnvelope
      +# modulatedCutoffFrequencyFromOscillatorB
      )
      *# k (settings ! "polyModDestinationFilter")

  cutoffFrequency =
    -- TODO: Is 22050 correct?
    modulatedCutoffFrequency
      *# k# 22050
      +# keyboardCutoffFrequency
      +# centerCutoffFrequency

  -- TODO: LFO mod

  finalCutoffFrequency = limit cutoffFrequency (k# 0) (k# 22050)
  resonance            = k (settings ! "filterResonance")
  filtered             = zdfLadder input finalCutoffFrequency resonance tival

lfoModule :: Map Text IRateSignal -> ARateSignal
lfoModule settings = oscillator where
  frequency =
    scaleWithDefaults (k (settings ! "lfoFrequency")) (k# 0.04) (k# 20)

  -- Possible shape values:
  -- 0 = disabled
  -- 1 = sawtooth
  -- 2 = triangle
  -- 3 = square
  shape                 = settings ! "lfoShape"

  triangleOrSquareCheck = ifEqualE (shape, i# 2) (i# 12, i# 10)
  baseMode              = ifEqualE (shape, i# 1) (i# 0, triangleOrSquareCheck)
  mode                  = baseMode +# tival

  lfo                   = vco2 (k# 1) frequency mode (k# 0) (k# 0) (i# 0.5)
  oscillator            = ifEqualE (shape, i# 0) (a# 0, lfo)

midiSettings :: Integer -> Map Text KRateSignal
midiSettings channel = fromList settings where
  [midiAmplitude, midiFrequency] = midiAmplitudeAndFrequency (k# channel)
  amplitude                      = ("amplitude", midiAmplitude)
  frequency                      = ("frequency", midiFrequency)
  toTuple (SynthParameter name _ minimum maximum _ _ midiCCNumber) =
    ( name
    , ctrl7WithDefaults (i# channel) (i# midiCCNumber) (k# minimum) (k# maximum)
    )
  settings = amplitude : frequency : map toTuple synthParameters

mixer :: Map Text IRateSignal -> ARateSignal -> ARateSignal -> ARateSignal
mixer settings oscA oscB = mix oscillators where
  oscBLevel   = k (settings ! "mixerOscillatorBLevel")
  oscALevel   = k (settings ! "mixerOscillatorALevel")
  noiseLevel  = k (settings ! "mixerNoiseLevel")

  noiseOutput = noise (k# 1) (k# 0)

  oscillators =
    [(oscA, oscALevel), (oscB, oscBLevel), (noiseOutput, noiseLevel)]

oscillatorA
  :: Map Text IRateSignal
  -> KRateSignal
  -> KRateSignal
  -> KRateSignal
  -> ARateSignal
oscillatorA settings oscBFrequency frequency pulseWidth = output where
  amplitude                = k (settings ! "amplitude")
  sync                     = k (settings ! "oscillatorASync")

  sawtoothActivated        = settings ! "oscillatorASawtooth"
  pulseActivated           = settings ! "oscillatorAPulse"

  activatedInstruments     = sawtoothActivated +# pulseActivated

  noSync                   = init (i# 0) :: ARateSignal
  phase                    = tival *# i# (-1)
  [_         , masterSync] = syncphasor oscBFrequency noSync phase
  [slavePhase, _         ] = syncphasor frequency masterSync phase

  oscPhase                 = k slavePhase *# sync

  sawtoothOsc              = sawtooth oscPhase amplitude frequency
  pulseOsc                 = pulse oscPhase pulseWidth amplitude frequency
  -- TODO: Account for polarity in other waveforms
  unipolarPulse            = limit pulseOsc (k# 0) amplitude

  mixed =
    mix [(sawtoothOsc, k sawtoothActivated), (unipolarPulse, k pulseActivated)]
  output =
    ifEqualE (activatedInstruments, i# 0) (a# 0, mixed /# activatedInstruments)

oscillatorAFrequency
  :: Map Text IRateSignal -> ARateSignal -> ARateSignal -> KRateSignal
oscillatorAFrequency settings oscB lfoSignal = oscAFrequency where
  frequency                = k (settings ! "frequency")

  semitoneAdjustment       = settings ! "oscillatorAFrequency"

  maximumSemitoneDeviation = k# 24

  envelope                 = filterEnvelope settings

  maximumSemitonesFromFilterEnvelope =
    k (settings ! "polyModSourceAmountFilterEnvelope")
      *# maximumSemitoneDeviation
  modulatedSemitonesFromFilterEnvelope =
    maximumSemitonesFromFilterEnvelope *# envelope

  maximumSemitonesFromOscillatorB =
    k (settings ! "polyModSourceAmountOscillatorB") *# maximumSemitoneDeviation
  modulatedSemitonesFromOscillatorB = maximumSemitonesFromOscillatorB *# k oscB

  polyModSemitoneAdjustment =
    (modulatedSemitonesFromFilterEnvelope +# modulatedSemitonesFromOscillatorB)
      *# k (settings ! "polyModDestinationFrequencyA")

  noiseSignal                 = pinker
  (lfoAmount, noiseAmount)    = wheelModSourceAmount settings

  maximumSemitonesFromLfo     = lfoAmount *# maximumSemitoneDeviation
  modulatedSemitonesFromLfo   = maximumSemitonesFromLfo *# k lfoSignal

  maximumSemitonesFromNoise   = noiseAmount *# maximumSemitoneDeviation
  modulatedSemitonesFromNoise = maximumSemitonesFromNoise *# k noiseSignal

  wheelModSemitoneAdjustment =
    (modulatedSemitonesFromLfo +# modulatedSemitonesFromNoise)
      *# (settings ! "wheelModDestinationFrequencyA")

  modSemitoneAdjustment =
    polyModSemitoneAdjustment +# wheelModSemitoneAdjustment

  totalSemitoneAdjustment = modSemitoneAdjustment +# semitoneAdjustment
  semitones = int# $ limit totalSemitoneAdjustment (k# (-24)) (k# 24)

  fineTuningCentsAdjustment = k# 0

  oscAFrequency =
    calculateFrequency frequency semitones fineTuningCentsAdjustment

oscillatorAPulseWidth
  :: Map Text IRateSignal -> ARateSignal -> ARateSignal -> KRateSignal
oscillatorAPulseWidth settings oscB lfoSignal = scaledPulseWidth where
  defaultPulseWidth = k (settings ! "oscillatorAPulseWidth")
  maximumPulseWidth = k# 1

  envelope          = filterEnvelope settings

  modulatedPulseWidthFromFilterEnvelope =
    settings ! "polyModSourceAmountFilterEnvelope" *# envelope

  modulatedPulseWidthFromOscillatorB =
    settings ! "polyModSourceAmountOscillatorB" *# k oscB

  polyModPulseWidth =
    (modulatedPulseWidthFromFilterEnvelope +# modulatedPulseWidthFromOscillatorB
      )
      *# (settings ! "polyModDestinationPulseWidthA")

  noiseSignal                  = pinker
  (lfoAmount, noiseAmount)     = wheelModSourceAmount settings

  maximumPulseWidthFromLfo     = lfoAmount *# maximumPulseWidth
  modulatedPulseWidthFromLfo   = maximumPulseWidthFromLfo *# k lfoSignal

  maximumPulseWidthFromNoise   = noiseAmount *# maximumPulseWidth
  modulatedPulseWidthFromNoise = maximumPulseWidthFromNoise *# k noiseSignal

  wheelModPulseWidth =
    (modulatedPulseWidthFromLfo +# modulatedPulseWidthFromNoise)
      *# (settings ! "wheelModDestinationPulseWidthA")

  pulseWidth = polyModPulseWidth +# wheelModPulseWidth +# defaultPulseWidth

  scaledPulseWidth = limit pulseWidth (k# 0) (k# 1)

oscillatorB
  :: Map Text IRateSignal -> KRateSignal -> KRateSignal -> ARateSignal
oscillatorB settings frequency pulseWidth = output where
  amplitude         = k (settings ! "amplitude")

  sawtoothActivated = settings ! "oscillatorBSawtooth"
  triangleActivated = settings ! "oscillatorBTriangle"
  pulseActivated    = settings ! "oscillatorBPulse"

  activatedInstruments =
    (sawtoothActivated +# triangleActivated :: IRateSignal) +# pulseActivated :: IRateSignal

  sawtoothOsc   = sawtooth (k# 0) amplitude frequency
  triangleOsc   = triangle (k# 0) amplitude frequency
  pulseOsc      = pulse (k# 0) pulseWidth amplitude frequency
  unipolarPulse = limit pulseOsc (k# 0) amplitude

  mixed         = mix
    [ (sawtoothOsc  , k sawtoothActivated)
    , (triangleOsc  , k triangleActivated)
    , (unipolarPulse, k pulseActivated)
    ]

  output =
    ifEqualE (activatedInstruments, i# 0) (a# 0, mixed /# activatedInstruments)

oscillatorBFrequency :: Map Text IRateSignal -> ARateSignal -> KRateSignal
oscillatorBFrequency settings lfoSignal = frequency where
  oscBFrequency = oscillatorBSemitonesToFrequency settings lfoSignal
  lfoFrequency  = oscillatorBLfoToFrequency settings lfoSignal

  frequency =
    ifEqualE (settings ! "oscillatorBLfo", i# 1) (lfoFrequency, oscBFrequency)

oscillatorBPulseWidth :: Map Text IRateSignal -> ARateSignal -> KRateSignal
oscillatorBPulseWidth settings lfoSignal = scaledPulseWidth where
  defaultPulseWidth            = k (settings ! "oscillatorBPulseWidth")
  maximumPulseWidth            = k# 1

  noiseSignal                  = pinker
  (lfoAmount, noiseAmount)     = wheelModSourceAmount settings

  maximumPulseWidthFromLfo     = lfoAmount *# maximumPulseWidth
  modulatedPulseWidthFromLfo   = maximumPulseWidthFromLfo *# k lfoSignal

  maximumPulseWidthFromNoise   = noiseAmount *# maximumPulseWidth
  modulatedPulseWidthFromNoise = maximumPulseWidthFromNoise *# k noiseSignal

  wheelModPulseWidth =
    (modulatedPulseWidthFromLfo +# modulatedPulseWidthFromNoise)
      *# (settings ! "wheelModDestinationPulseWidthB")

  pulseWidth       = wheelModPulseWidth +# defaultPulseWidth

  scaledPulseWidth = limit pulseWidth (k# 0) (k# 1)

oscillatorBLfoToFrequency :: Map Text IRateSignal -> ARateSignal -> KRateSignal
oscillatorBLfoToFrequency settings lfoSignal = lfoFrequency where
  oscBFrequency              = settings ! "oscillatorBFrequency"

  minimumLfoFrequency        = 0.4
  maximumLfoFrequency        = 30

  asLfoFrequency             = scaleLfoFrequency oscBFrequency

  noiseSignal                = pinker
  (lfoAmount, noiseAmount)   = wheelModSourceAmount settings

  maximumModulationFromLfo   = lfoAmount *# k# maximumLfoFrequency
  modulationFromLfo          = maximumModulationFromLfo *# k lfoSignal

  maximumModulationFromNoise = noiseAmount *# k# maximumLfoFrequency
  modulationFromNoise        = maximumModulationFromNoise *# k noiseSignal

  wheelModModulation =
    (modulationFromLfo +# modulationFromNoise)
      *# (settings ! "wheelModDestinationFrequencyB")

  modulatedFrequency = wheelModModulation +# asLfoFrequency

  lfoFrequency =
    limit modulatedFrequency (k# minimumLfoFrequency) (k# maximumLfoFrequency)

oscillatorBSemitonesToFrequency
  :: Map Text IRateSignal -> ARateSignal -> KRateSignal
oscillatorBSemitonesToFrequency settings lfoSignal = calculatedFrequency where
  frequency            = k (settings ! "frequency")
  frequencyAsSemitones = k (settings ! "oscillatorBFrequency")
  fineTuningCentsAdjustment =
    k (settings ! "oscillatorBFineTuningCentsAdjustment")

  minimumSemitones           = k# (-24)
  maximumSemitones           = k# 24

  noiseSignal                = pinker
  (lfoAmount, noiseAmount)   = wheelModSourceAmount settings

  maximumModulationFromLfo   = lfoAmount *# maximumSemitones
  modulationFromLfo          = maximumModulationFromLfo *# k lfoSignal

  maximumModulationFromNoise = noiseAmount *# maximumSemitones
  modulationFromNoise        = maximumModulationFromNoise *# k noiseSignal

  wheelModModulation =
    (modulationFromLfo +# modulationFromNoise)
      *# (settings ! "wheelModDestinationFrequencyB")

  modulatedSemitones = wheelModModulation +# frequencyAsSemitones

  semitones = int# $ limit modulatedSemitones minimumSemitones maximumSemitones

  calculatedFrequency =
    calculateFrequency frequency semitones fineTuningCentsAdjustment

profit :: Map Text IRateSignal -> ARateSignal
profit parameters = ARateSignal $ Signal opcode 1 where
  settings = merge defaultSettings parameters
  inputs   = map getSignal (elems settings)
  opcode   = UserDefinedOpcode udo inputs

pulse
  :: KRateSignal -> KRateSignal -> KRateSignal -> KRateSignal -> ARateSignal
pulse phase width amplitude frequency =
  let mode = i# 18 +# tival
  in  vco2 amplitude frequency mode width phase (i# 0.5)

sawtooth :: KRateSignal -> KRateSignal -> KRateSignal -> ARateSignal
sawtooth phase amplitude frequency =
  let mode = i# 16 +# tival
  in  vco2 amplitude frequency mode (k# 0) phase (i# 0.5)

scaleCutoffFrequency
  :: KRateSignal -> KRateSignal -> KRateSignal -> KRateSignal
scaleCutoffFrequency lowerBound upperBound value = result where
  -- freq = exp(ln(8.3) + knob * (ln(8499.25) - ln(8.3)))
  logLowerBound    = log# lowerBound
  difference       = log# upperBound -# logLowerBound
  secondExpression = value *# difference
  exponent         = logLowerBound +# secondExpression
  result           = k# e ^# exponent

-- NOTE: See https://www.wolframalpha.com/input/?i=exponential+fit+%7B%7B0%2C+0.001%7D%2C+%7B0.5%2C+0.5%7D%2C+%7B1%2C+30%7D%7D
scaleEnvelopeKnob :: IRateSignal -> IRateSignal
scaleEnvelopeKnob value = result where
  -- 0.0083252 e^(8.18967 x)
  exponent      = i# 8.18967 *# value
  eWithExponent = i# e ^# exponent
  result        = i# 0.0083252 *# eWithExponent

-- NOTE: See https://www.wolframalpha.com/input/?i=exponential+fit+%7B%7B-24%2C+0.4%7D%2C+%7B0%2C+1.05%7D%2C+%7B24%2C+30%7D%7D
scaleLfoFrequency :: IRateSignal -> IRateSignal
scaleLfoFrequency value = result where
  -- 1.07592 e^(0.138667 x)
  exponent      = i# 0.138667 *# value
  eWithExponent = i# e ^# exponent
  result        = i# 1.07592 *# eWithExponent

synth :: Map Text IRateSignal -> ARateSignal
synth settings = output where
  lfoSignal      = lfoModule settings

  oscBFrequency  = oscillatorBFrequency settings lfoSignal
  oscBPulseWidth = oscillatorBPulseWidth settings lfoSignal
  oscB           = oscillatorB settings oscBFrequency oscBPulseWidth

  oscAFrequency  = oscillatorAFrequency settings oscB lfoSignal
  oscAPulseWidth = oscillatorAPulseWidth settings oscB lfoSignal

  oscA = oscillatorA settings oscBFrequency oscAFrequency oscAPulseWidth
  mixed          = mixer settings oscA oscB

  filtered       = filterInput settings mixed oscB

  output         = filtered *# amplifierEnvelope settings

synthFields :: [Text]
synthFields =
  sort $ "amplitude" : "frequency" : map synthParameterName synthParameters

synthParameters :: [SynthParameter]
synthParameters =
  [ SynthParameter "oscillatorBFrequency" "Osc B Freq" (-24) 24 1 0 75
  , SynthParameter "oscillatorBFineTuningCentsAdjustment"
                   "Osc B Tuning"
                   (-50)
                   50
                   1
                   0
                   76
  , SynthParameter "oscillatorBPulseWidth" "Osc B Pulse Width" 0 1 0 0.5 79
  , SynthParameter "oscillatorBSawtooth"   "Osc B Sawtooth"    0 1 1 0   23
  , SynthParameter "oscillatorBTriangle"   "Osc B Triangle"    0 1 1 1   24
  , SynthParameter "oscillatorBPulse"      "Osc B Pulse"       0 1 1 0   25
  , SynthParameter "oscillatorBLfo"        "Osc B LFO"         0 1 1 0   26
  , SynthParameter "polyModSourceAmountFilterEnvelope"
                   "Poly Mod Filter Amt"
                   0
                   1
                   0
                   0
                   27
  , SynthParameter "polyModSourceAmountOscillatorB"
                   "Poly Mod Osc B Amt"
                   0
                   1
                   0
                   0
                   28
  , SynthParameter "polyModDestinationFrequencyA"
                   "Poly Mod To Freq A"
                   0
                   1
                   1
                   0
                   29
  , SynthParameter "polyModDestinationPulseWidthA" "Poly Mod to PW A" 0 1 1 0 30
  , SynthParameter "polyModDestinationFilter" "Poly Mod to Filter" 0 1 1 0 31
  , SynthParameter "lfoFrequency" "LFO Freq" 0 1 0 0 88
  , SynthParameter "lfoShape" "LFO Shape" 0 3 1 0 90
  , SynthParameter "wheelModSourceAmount" "Wheel Mod LFO Amt" 0 1 0 0 33
  , SynthParameter "wheelModDestinationFrequencyA"
                   "Wheel Mod To Freq A"
                   0
                   1
                   1
                   0
                   34
  , SynthParameter "wheelModDestinationFrequencyB"
                   "Wheel Mod To Freq B"
                   0
                   1
                   1
                   0
                   35
  , SynthParameter "wheelModDestinationPulseWidthA"
                   "Wheel Mod To PW A"
                   0
                   1
                   1
                   0
                   36
  , SynthParameter "wheelModDestinationPulseWidthB"
                   "Wheel Mod To PW B"
                   0
                   1
                   1
                   0
                   37
  , SynthParameter "wheelModDestinationFilter" "Wheel Mod To Filter" 0 1 1 0 38
  , SynthParameter "oscillatorAFrequency" "Osc A Freq" (-24) 24 1 0 67
  , SynthParameter "oscillatorAPulseWidth" "Osc A Pulse Width" 0 1 0 0.5 71
  , SynthParameter "oscillatorASawtooth" "Osc A Sawtooth" 0 1 1 1 20
  , SynthParameter "oscillatorAPulse" "Osc A Pulse" 0 1 1 0 21
  , SynthParameter "oscillatorASync" "Osc A Sync" 0 1 1 0 22
  , SynthParameter "mixerOscillatorBLevel" "Osc B Level" 0 1 0 0.5 77
  , SynthParameter "mixerOscillatorALevel" "Osc A Level" 0 1 0 0.5 69
  , SynthParameter "mixerNoiseLevel" "Noise Level" 0 1 0 0 32
  , SynthParameter "filterCutoffFrequency" "Filter CF" 0 1 0 0.5 102
  , SynthParameter "filterResonance" "Filter Res" 0 25 0 10 103
  , SynthParameter "filterEnvelopeAmount" "Filter Env" 0 1 0 0.5 105
  , SynthParameter "filterKeyboardAmount" "Filter Kbd" 0 1 0 0.5 104
  , SynthParameter "filterAttack" "Filter Att" 0 1 0 0.5 50
  , SynthParameter "filterDecay" "Filter Dec" 0 1 0 0.5 51
  , SynthParameter "filterSustain" "Filter Sus" 0 1 0 0.5 52
  , SynthParameter "filterRelease" "Filter Rel" 0 1 0 0.5 53
  , SynthParameter "amplifierAttack" "Amp Att" 0 1 0 0.05 43
  , SynthParameter "amplifierDecay" "Amp Dec" 0 1 0 0.1 44
  , SynthParameter "amplifierSustain" "Amp Sus" 0 1 0 0.4 45
  , SynthParameter "amplifierRelease" "Amp Rel" 0 1 0 0.1 46
  , SynthParameter "wheelModAmount" "Wheel Mod" 0 1 0 0 1
  ]

triangle :: KRateSignal -> KRateSignal -> KRateSignal -> ARateSignal
triangle phase amplitude frequency =
  let mode = i# 28 +# tival
  in  vco2 amplitude frequency mode (k# 0) phase (i# 0.5)

udo :: Udo
udo = userDefinedOpcode where
  inputRates        = replicate (length synthFields) IRate
  inputs            = xin inputRates
  parameters        = fromList $ zip synthFields (map makeRatedSignal inputs)
  output            = synth parameters
  opcode            = TerminalOpcode $ Op $ xout [getSignal output]
  userDefinedOpcode = Udo { udoName                 = "profit"
                          , udoInputRates           = inputRates
                          , udoOutputRates          = [ARate]
                          , udoOpcode               = opcode
                          , udoControlPeriodSamples = 0
                          }

wheelModSourceAmount :: Map Text IRateSignal -> (IRateSignal, IRateSignal)
wheelModSourceAmount settings = sourceAmount where
  wheelModAmount = settings ! "wheelModAmount"
  noiseAmount    = settings ! "wheelModSourceAmount"
  lfoAmount      = i# 1 -# noiseAmount :: IRateSignal
  sourceAmount   = (lfoAmount *# wheelModAmount, noiseAmount *# wheelModAmount)
