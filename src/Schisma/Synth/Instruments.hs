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
import           Schisma.Csound.Opcodes.MidiInput
                                                ( triggerMidiNote )
import           Schisma.Csound.Opcodes.SampleLevelOperators
                                                ( i )
import           Schisma.Csound.Opcodes.SignalOutput
                                                ( out )
import           Schisma.Csound.Opcodes.TableReadWriteOperations
                                                ( tablewWithDefaults )

import           Schisma.Csound.GenRoutines     ( gen02 )
import           Schisma.Csound.Utilities       ( passthrough )

import           Schisma.Csound.Types.Instruments
                                                ( Instrument(..) )
import           Schisma.Csound.Types.Signals   ( KRateSignal
                                                , Opcode(TerminalOpcode)
                                                , OrdinaryStatement(Op)
                                                )

import           Schisma.Utilities              ( foldlWithIndex )

midiTrigger :: Map Text KRateSignal -> Integer -> Integer -> Instrument
midiTrigger parameters midiChannel instrumentNumber = Instrument
  { instrumentOpcode   = opcode
  , instrumentNumber   = sourceInstrumentNumber
  , instrumentAlwaysOn = True
  } where
  -- NOTE: We're setting the duration to -1000000000 so that opcode
  -- tieStatus correctly classifies this note as a standalone note, and not
  -- as an initial note within a group of tied notes. This is important
  -- because any initial notes won't have any release segments attached to
  -- them, whereas standalone notes do.
  duration                = -1000000000
  nonRatedEventParameters = [instrumentNumber, 0, duration]
  eventOnParameters       = map k# nonRatedEventParameters ++ elems parameters

  notesStateParameters    = i# instrumentNumber : replicate 257 (i# 0)

  eventOnParameterTable =
    gen02 False (map i# nonRatedEventParameters ++ map i (elems parameters))

  tableWrite ftn index signal =
    passthrough ftn (tablewWithDefaults signal (k# index) eventOnParameterTable)
  eventTable =
    foldlWithIndex tableWrite eventOnParameterTable eventOnParameters

  notesStateTable = gen02 False notesStateParameters

  opcode = triggerMidiNote (k# midiChannel) eventTable notesStateTable
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
profit instrumentNumber = Instrument { instrumentOpcode   = opcode
                                     , instrumentNumber   = instrumentNumber
                                     , instrumentAlwaysOn = False
                                     }
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
soundFontPlayer file instrumentNumber = Instrument
  { instrumentOpcode   = opcode
  , instrumentNumber   = instrumentNumber
  , instrumentAlwaysOn = False
  }
 where
  pFieldParameters = patchFieldsToPFieldParameters SoundFont.synthFields
  sf               = SoundFont.player pFieldParameters (stringSignal file)
  opcode           = TerminalOpcode $ Op $ out sf
