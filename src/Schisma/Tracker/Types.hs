module Schisma.Tracker.Types where

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )

import           Schisma.Csound.Types           ( Instrument
                                                , Sound
                                                )

data NoteCell
  = NoteOn Text (Map Text Double)
  --       pitch  instrument effects
  | NoteOff
  | NoteBlank (Map Text Double)
  --          instrument effects
  deriving (Show, Ord, Eq)

data MasterSettingsState = MasterSettingsState
  { trackerTuning :: Text
  , trackerTemperament :: Text
  , trackerBeatsPerMinute :: Double
  , trackerLinesPerBeat :: Double }
  deriving (Show, Ord, Eq)

data MasterSetting
  = BeatsPerMinute Double
  | LinesPerBeat Double
  | Tuning Text
  | Temperament Text
  deriving (Show, Ord, Eq)

data MasterCell
  = MasterSettings [MasterSetting]
  | MasterBlankSettings
  deriving (Show, Ord, Eq)

data HeaderCell
  = LineNumberHeader
  | MasterHeader
  | InstrumentHeader Integer Bool  Bool
  --                 number  muted soloed
  deriving (Show, Ord, Eq)

data InstrumentTrack = InstrumentTrack
  { trackerTrackInstrumentNumber :: Integer
  , trackerTrackIsMute :: Bool
  , trackerTrackIsSolo :: Bool
  , trackerTrackNotes :: [NoteCell]
  }
  deriving (Show, Ord, Eq)

data CellMappers = CellMappers
  { cellFrequencyMapper :: Integer -> Text -> Text -> Text -> Map Text Double
  , cellParameterRenamer :: Map Text Double -> Map Text Double
  }

data Tracker
  = Tracker [Integer] [MasterCell] [InstrumentTrack]
  --        line #    master track instrument tracks
  deriving (Show, Ord, Eq)

data TrackerPlaybackState = TrackerPlaybackState
  { trackerPlaybackTimeElapsed :: Double
  , trackerPlaybackPriorSounds :: [Sound]
  , trackerPlaybackPriorLineNumber :: Integer
  , trackerPlaybackPriorMasterSettingsState :: MasterSettingsState
  , trackerPriorNoteConcluded :: Bool
  , trackerPriorPitch :: Maybe Text
  }

data TrackerFileConfiguration = TrackerFileConfiguration
  { trackerCellMappers :: CellMappers
  , trackerInstruments :: [Instrument]
  , trackerInstrumentParameters :: Map Integer (Map Text Double)
  , trackerLineConstraints :: (Integer, Integer)
  }
