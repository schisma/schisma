module Schisma.Synth.Instruments
  ( midiProfit
  , midiSoundFontPlayer
  , profit
  , soundFontPlayer
  ) where

import           Data.Map.Strict                ( Map
                                                , elems
                                                )
import           Data.Text                      ( Text )

import qualified Schisma.Synth.SoundFont       as SoundFont
                                                ( midiSettings
                                                , player
                                                , synthFields
                                                )

import qualified Schisma.Synth.Emulations.Profit
                                               as Profit
                                                ( midiSettings
                                                , profit
                                                , synthFields
                                                )

import           Schisma.Synth.Emulations.Patches
                                                ( patchFieldsToPFieldParameters
                                                )


import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , k#
                                                , stringSignal
                                                )

import           Schisma.Csound.Opcodes.Arithmetic
                                                ( (*#)
                                                , (+#)
                                                )
import           Schisma.Csound.Opcodes.InstrumentDuration
                                                ( turnoff2 )
import           Schisma.Csound.Opcodes.InstrumentInvocation
                                                ( Event(..) )
import           Schisma.Csound.Opcodes.Logic   ( ifEqualS )
import           Schisma.Csound.Opcodes.MidiInput
                                                ( isMidiNotePlaying
                                                , midiChannelMatches
                                                )
import           Schisma.Csound.Opcodes.SampleLevelOperators
                                                ( i )
import           Schisma.Csound.Opcodes.SignalOutput
                                                ( out )
import           Schisma.Csound.Opcodes.TableQueries
                                                ( ftlen )

import           Schisma.Csound.GenRoutines     ( gen02 )

import           Schisma.Csound.Types.Instruments
                                                ( Instrument(..) )
import           Schisma.Csound.Types.Signals   ( KRateSignal
                                                , Opcode(TerminalOpcode)
                                                , OrdinaryStatement(NoOp, Op)
                                                )

midiTrigger :: Map Text KRateSignal -> Integer -> Integer -> Instrument
midiTrigger parameters midiChannel instrumentNumber = Instrument
  opcode
  sourceInstrumentNumber where

  [playing, noteNumber]  = isMidiNotePlaying (k# midiChannel)

  targetInstrumentNumber = k# instrumentNumber +# (noteNumber *# k# 0.000001)
  eventOnParameters      = elems parameters

  table                  = gen02 False (map i eventOnParameters)

  -- NOTE: We're setting the duration to -1000000000 so that opcode
  -- tieStatus correctly classifies this note as a standalone note, and not
  -- as an initial note within a group of tied notes. This is important
  -- because any initial notes won't have any release segments attached to
  -- them, whereas standalone notes do.
  turnOn                 = event (stringSignal "i")
                                 targetInstrumentNumber
                                 (k# 0)
                                 (k# (-1000000000))
                                 eventOnParameters

  turnOff        = turnoff2 targetInstrumentNumber (k# 12) (k# 1)

  turnOnOrOff    = ifEqualS (playing, k# 1) (Op turnOn, Op turnOff)

  checkIfPlaying = ifEqualS (playing, k# 0) (NoOp, turnOnOrOff)

  trigger        = ifEqualS (ftlen table   , i# (length eventOnParameters))
                            (checkIfPlaying, NoOp)

  opcode                 = TerminalOpcode trigger
  sourceInstrumentNumber = instrumentNumber + 1000

-- | Transforms the Profit synthesizer into a Csound Instrument. The
--   synthesizer parameters can be set via MIDI.
midiProfit
  :: Integer    -- ^ @channel@ - The MIDI channel number.
  -> Integer    -- ^ @instrumentNumber@ - The Instrument number.
  -> Instrument -- ^ The Instrument.
midiProfit channel = midiTrigger (Profit.midiSettings channel) channel

-- | Transforms the Profit synthesizer into a Csound Instrument. The
--   synthesizer parameters can be set via p-fields.
profit
  :: Integer    -- ^ @instrumentNumber@ - The Instrument number.
  -> Instrument -- ^ The Instrument.
profit = Instrument opcode
 where
  pFieldParameters = patchFieldsToPFieldParameters Profit.synthFields
  synth            = Profit.profit pFieldParameters
  opcode           = TerminalOpcode $ Op $ out [synth, synth]

-- | Transforms the SoundFont player into a Csound Instrument. The player
--   parameters can be set via MIDI.
midiSoundFontPlayer
  :: Integer    -- ^ @channel@ - The MIDI channel number.
  -> Integer    -- ^ @instrumentNumber@ - The Instrument number.
  -> Instrument -- ^ The Instrument.
midiSoundFontPlayer channel =
  midiTrigger (SoundFont.midiSettings channel) channel

-- | Transforms the SoundFont player into a Csound Instrument. The player
--   parameters can be set via p-fields.
soundFontPlayer
  :: Text       -- ^ @file@ - The path to the SF2 file.
  -> Integer    -- ^ @instrumentNumber@ - The Instrument number.
  -> Instrument -- ^ The Instrument.
soundFontPlayer file = Instrument opcode
 where
  pFieldParameters = patchFieldsToPFieldParameters SoundFont.synthFields
  sf               = SoundFont.player pFieldParameters (stringSignal file)
  opcode           = TerminalOpcode $ Op $ out sf
