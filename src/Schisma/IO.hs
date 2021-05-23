module Schisma.IO
  ( playSounds
  , playTrackerFile
  , playWithMidiDevice
  , playWithVirtualMidi
  ) where

import           Data.Map.Strict                ( Map
                                                , delete
                                                , empty
                                                )
import           Data.Text                      ( Text )
import           System.Directory               ( getTemporaryDirectory )
import           System.FilePath                ( (</>)
                                                , takeBaseName
                                                )
import           System.Process                 ( callCommand )

import           Schisma.Csound.Opcodes.SignalOutput
                                                ( out )

import           Schisma.Csound.Score           ( alwaysOnIStatement
                                                , fZeroStatement
                                                , soundToIStatement
                                                )

import           Schisma.Csound.Types.Csound    ( Csd(..) )
import           Schisma.Csound.Types.Instruments
                                                ( Instrument
                                                  ( Instrument
                                                  , instrumentAlwaysOn
                                                  , instrumentNumber
                                                  , instrumentOpcode
                                                  )
                                                )
import           Schisma.Csound.Types.Score     ( Sound )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , Opcode(TerminalOpcode)
                                                , OrdinaryStatement(Op)
                                                )


import           Schisma.Tracker.Parser         ( parseTrackerFile )
import           Schisma.Tracker.Translator     ( trackerToScore )
import           Schisma.Tracker.Types          ( CellMappers
                                                , TrackerFileConfiguration(..)
                                                )

import           Schisma.Csound.Renderer        ( defaultOrchestraHeaderStatements
                                                , renderCsd
                                                )

import           Schisma.Utilities              ( merge )

import           ProjectPaths                   ( getIncludesDir )

midiCsd :: Map Text Text -> Bool -> [ARateSignal] -> Csd
midiCsd headerStatements useVirtual signals =
  let
    opcode     = TerminalOpcode $ Op $ out signals
    instrument = Instrument { instrumentOpcode   = opcode
                            , instrumentNumber   = 1
                            , instrumentAlwaysOn = False
                            }
    score   = [fZeroStatement 36000]
    options = if useVirtual
      then "-odac -+rtmidi=virtual -M0"
      else "-odac -+rtmidi=portmidi -Ma"
    header = merge (delete "massign" defaultOrchestraHeaderStatements)
                   headerStatements
  in
    Csd { csdOptions                   = options
        , csdOrchestraHeaderStatements = header
        , csdInstruments               = [instrument]
        , csdScore                     = score
        }

-- | Compiles the @signals@ into an instrument and the @sounds@ into a score
--   and then runs the resulting Csound file.
playSounds
  :: Map Text Text -- ^ @headerStatements@ - The map containing the header
                   --   statements.
  -> [ARateSignal] -- ^ @signals@ - The signals.
  -> [Sound]       -- ^ @sounds@ - The sounds.
  -> IO ()         -- ^
playSounds headerStatements signals sounds = do
  let opcode = TerminalOpcode $ Op $ out signals
  let instrument = Instrument { instrumentOpcode   = opcode
                              , instrumentNumber   = 1
                              , instrumentAlwaysOn = False
                              }
  let score  = map (soundToIStatement (1, empty)) sounds
  let header = merge defaultOrchestraHeaderStatements headerStatements
  let csd = Csd { csdOptions                   = "-odac"
                , csdOrchestraHeaderStatements = header
                , csdInstruments               = [instrument]
                , csdScore                     = score
                }
  tmpDirectory <- getTemporaryDirectory
  let filename = tmpDirectory </> "sounds.csd"

  renderCsd csd filename
  runCsd filename

-- | Compiles the tracker @file@ and runs the resulting Csound file.
playTrackerFile
  :: Map Text Text            -- ^ @headerStatements@ - The map containing the
                              --   header statements.
  -> [Integer]                -- ^ @alwaysOnInstrumentNumbers@ - The numbers of
                              --   the instrument that should always be on
  -> FilePath                 -- ^ @file@ - The path to the tracker file.
  -> TrackerFileConfiguration -- ^ @trackerConfiguration@ - The tracker
                              --   configuration.
  -> IO ()                    -- ^
playTrackerFile headerStatements alwaysOnInstrumentNumbers file trackerConfiguration
  = do
    tracker <- parseTrackerFile file

    let options      = "-odac -+rtmidi=portmidi -Ma"
    let header       = merge defaultOrchestraHeaderStatements headerStatements

    let trackerScore = trackerToScore tracker trackerConfiguration
    let instruments  = trackerInstruments trackerConfiguration
    let score =
          map alwaysOnIStatement alwaysOnInstrumentNumbers ++ trackerScore
    let csd = Csd { csdOptions                   = options
                  , csdOrchestraHeaderStatements = header
                  , csdInstruments               = instruments
                  , csdScore                     = score
                  }
    tmpDirectory <- getTemporaryDirectory
    let csdFilename = tmpDirectory </> (takeBaseName file ++ ".csd")

    renderCsd csd csdFilename
    runCsd csdFilename

-- | Compiles the @signals@ into an instrument and runs the resulting Csound
--   file with MIDI interoperability enabled.
--
--   <https://csound.com/docs/manual/MidiTop.html Csound documentation>
playWithMidiDevice
  :: Map Text Text -- ^ @headerStatements@ - The map containing the header
                   --   statements.
  -> [ARateSignal] -- ^ @signals@ - The signals.
  -> IO ()         -- ^
playWithMidiDevice headerStatements signals = do
  let csd = midiCsd headerStatements False signals
  tmpDirectory <- getTemporaryDirectory
  let filename = tmpDirectory </> "midi_device.csd"

  renderCsd csd filename
  runCsd filename

-- | Compiles the @signals@ into an instrument and runs the resulting Csound
--   file with a virtual MIDI keyboard.
--
--   <https://csound.com/docs/manual/MidiTop.html Csound documentation>
playWithVirtualMidi
  :: Map Text Text -- ^ @headerStatements@ - The map containing the header
                   --   statements.
  -> [ARateSignal] -- ^ @signals@ - The signals.
  -> IO ()         -- ^
playWithVirtualMidi headerStatements signals = do
  let csd = midiCsd headerStatements True signals
  tmpDirectory <- getTemporaryDirectory
  let filename = tmpDirectory </> "virtual_midi.csd"

  renderCsd csd filename
  runCsd filename

runCsd :: FilePath -> IO ()
runCsd filename = do
  includesDir <- getIncludesDir
  let envVariables = "INCDIR=" ++ show includesDir
  let command      = "csound " ++ filename
  callCommand $ envVariables ++ " " ++ command
