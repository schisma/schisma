module Schisma.Synth.SoundFont
  ( midiSettings
  , player
  , synthFields
  , synthParameters
  ) where

import           Data.List                      ( sort )
import           Data.Map.Strict                ( (!)
                                                , Map
                                                , fromList
                                                )
import           Data.Text                      ( Text )

import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , k#
                                                , pk#
                                                )

import           Schisma.Csound.GenRoutines     ( gen07 )
import           Schisma.Csound.Opcodes.Arithmetic
                                                ( (*#)
                                                , (-#)
                                                , (/#)
                                                , (^#)
                                                )
import           Schisma.Csound.Opcodes.EnvelopeGenerators
                                                ( adsrForTiedNotes )
import           Schisma.Csound.Opcodes.MidiConverters
                                                ( midiAmplitudeAndFrequency )
import           Schisma.Csound.Opcodes.MidiInput
                                                ( ctrl7WithDefaults )
import           Schisma.Csound.Opcodes.PitchConverters
                                                ( ftom )
import           Schisma.Csound.Opcodes.SamplePlayback
                                                ( sfload
                                                , sfplayForTiedNotes
                                                , sfpreset
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , IRateSignal
                                                , KRateSignal
                                                , SRateSignal
                                                )
import           Schisma.Synth.Types            ( SynthParameter
                                                  ( SynthParameter
                                                  , synthParameterName
                                                  )
                                                )

import           Schisma.Utilities              ( merge )

-- TODO: Doc

-- Determines the envelope based on the documentation provided on page 33
-- of https://www.synthfont.com/sfspec24.pdf.
adsrEnvelope
  :: IRateSignal -> IRateSignal -> IRateSignal -> IRateSignal -> KRateSignal
adsrEnvelope attack decay sustain release = envelope where
  scaledAttack  = scaleEnvelopeParameter attack
  scaledDecay   = scaleEnvelopeParameter decay
  scaledSustain = i# 1 -# (sustain /# i# 1000)
  scaledRelease = scaleEnvelopeParameter release

  envelope =
    adsrForTiedNotes scaledAttack scaledDecay scaledSustain scaledRelease

amplifierEnvelope :: Map Text IRateSignal -> KRateSignal
amplifierEnvelope settings = adsrEnvelope attack decay sustain release where
  attack  = settings ! "amplifierAttack"
  decay   = settings ! "amplifierDecay"
  sustain = settings ! "amplifierSustain"
  release = settings ! "amplifierRelease"

defaultSettings :: Map Text IRateSignal
defaultSettings = fromList settings where
  toTuple (SynthParameter name _ _ _ defaultValue _) =
    (name, i# defaultValue)
  settings = map toTuple synthParameters

midiSettings :: Integer -> Map Text KRateSignal
midiSettings channel = fromList settings where
  [midiAmplitude, midiFrequency] = midiAmplitudeAndFrequency (k# channel)
  amplitude                      = ("amplitude", midiAmplitude)
  frequency                      = ("frequency", midiFrequency)
  toTuple (SynthParameter name minimum maximum _ _ midiCCNumber) =
    ( name
    , ctrl7WithDefaults (i# channel) (i# midiCCNumber) (k# minimum) (k# maximum)
    )
  settings = amplitude : frequency : map toTuple synthParameters

player :: Map Text IRateSignal -> SRateSignal -> [ARateSignal]
player parameters fileSignal = output where
  settings         = merge defaultSettings parameters

  program          = settings ! "program"
  bank             = settings ! "bank"
  fileIdentifier   = sfload fileSignal
  presetIdentifier = sfpreset program bank fileIdentifier (i# 0)

  velocity         = settings ! "velocity"
  -- NOTE: The amplitude is scaled by the default value of 0dbfs (32768) in
  -- order to prevent deafening volumes.
  amplitude        = (settings ! "amplitude") /# i# 32768
  frequency        = settings ! "frequency"

  -- NOTE: When @frequencyFlag@ is 1, the frequency is determined not by
  -- @noteNumber@, but by @frequency@. Ideally, the nearest sample should
  -- be chosen, hence the conversion of @frequency@ to a MIDI note number.
  frequencyFlag    = i# 1
  noteNumber       = ftom frequency (i# 1)

  soundfontSignals = sfplayForTiedNotes velocity
                                        noteNumber
                                        amplitude
                                        frequency
                                        presetIdentifier
                                        frequencyFlag
                                        (i# 0)
                                        (i# 0)

  envelope = amplifierEnvelope settings

  output   = map (*# envelope) soundfontSignals

-- Scales the envelope parameter based on the documentation provided on
-- page 33 of https://www.synthfont.com/sfspec24.pdf.
scaleEnvelopeParameter :: IRateSignal -> IRateSignal
scaleEnvelopeParameter parameter = result where
  -- 2^(x / 1200)
  exponent = parameter /# i# 1200
  result   = i# 2 ^# exponent

synthFields :: [Text]
synthFields =
  sort $ "amplitude" : "frequency" : map synthParameterName synthParameters

synthParameters :: [SynthParameter]
synthParameters =
  [ SynthParameter "amplifierAttack"  (-32768) 32768 1 0 43
  , SynthParameter "amplifierDecay"   (-32768) 32678 1 0 44
  , SynthParameter "amplifierRelease" (-32768) 32678 1 0 45
  , SynthParameter "amplifierSustain" 0        1000  0 0 46
  , SynthParameter "bank"             0        128   1 0 0
  , SynthParameter "program"          0        127   1 0 70
  , SynthParameter "velocity"         0        127   1 0 75
  ]
