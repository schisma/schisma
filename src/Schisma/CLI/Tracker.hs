{-# LANGUAGE DeriveGeneric #-}

module Schisma.CLI.Tracker
  ( PlayTrackerOptions(..)
  , CompositionFileJSON(..)
  , InstrumentsFileJSON(..)
  , ProjectFileJSON(..)
  , TrackerSettingsJSON(..)
  , playTrackerOptionsParser
  , toInstruments
  , toInstrumentParameters
  ) where

import           Data.Aeson                     ( FromJSON )

import           GHC.Generics                   ( Generic )

import           Data.Text                      ( Text
                                                , unpack
                                                )

import           Data.Map.Strict                ( Map
                                                , fromList
                                                )

import           Options.Applicative            ( Parser
                                                , auto
                                                , help
                                                , long
                                                , metavar
                                                , option
                                                , short
                                                , strOption
                                                )

import           Data.List                      ( foldl' )

import           Schisma.Csound.Types.Instruments
                                                ( Instrument )

import           Schisma.CLI.TrackerSettings    ( FrequencyMapperJSON )
import           Schisma.Synth.Instruments      ( midiProfit
                                                , midiSoundFontPlayer
                                                , profit
                                                , soundFontPlayer
                                                )

-- TODO: Doc

data TrackerSettingsJSON = TrackerSettingsJSON
  { parameterRenamings :: Map Text Text
  , frequencyMapper    :: FrequencyMapperJSON
  }
  deriving (Generic, Show)

newtype CompositionFileJSON = CompositionFileJSON
  { trackerSettings :: TrackerSettingsJSON
  }
  deriving (Generic, Show)

data InstrumentJSON = InstrumentJSON
  { number        :: Integer
  , name          :: Text
  , instrument    :: Text
  , soundFontPath :: Text
  , parameters    :: Map Text Double
  , midiChannel   :: Integer
  }
  deriving (Generic, Show)

newtype InstrumentsFileJSON = InstrumentsFileJSON
  { instruments        :: [InstrumentJSON]
  }
  deriving (Generic, Show)

data ProjectFileJSON = ProjectFileJSON
  { compositionFile :: FilePath
  , instrumentsFile :: FilePath
  , trackerFile     :: FilePath
  }
  deriving (Generic, Show)

instance FromJSON TrackerSettingsJSON
instance FromJSON CompositionFileJSON
instance FromJSON InstrumentJSON
instance FromJSON InstrumentsFileJSON
instance FromJSON ProjectFileJSON


data PlayTrackerOptions = PlayTrackerOptions
  { projectFile  :: FilePath
  , startingLine :: Integer
  , endingLine   :: Integer
  }
  deriving Show


playTrackerOptionsParser :: Parser PlayTrackerOptions
playTrackerOptionsParser =
  PlayTrackerOptions
    <$> strOption
          (long "project-file" <> short 'p' <> metavar "FILENAME" <> help
            "Project file"
          )
    <*> option
          auto
          (  long "start"
          <> short 's'
          <> help "The starting line number"
          <> metavar "INT"
          )
    <*> option
          auto
          (long "end" <> short 'e' <> help "The ending line number" <> metavar
            "INT"
          )

toInstruments :: InstrumentJSON -> [Instrument]
toInstruments json = schismaInstrument
 where
  instrumentName    = instrument json
  instrumentNumber  = number json
  channel           = midiChannel json
  schismaInstrument = case instrumentName of
    "Profit" -> if channel /= 0
      then [profit instrumentNumber, midiProfit channel instrumentNumber]
      else [profit instrumentNumber]
    "SoundFont" -> if channel /= 0
      then
        [ soundFontPlayer (soundFontPath json) instrumentNumber
        , midiSoundFontPlayer channel instrumentNumber
        ]
      else [soundFontPlayer (soundFontPath json) instrumentNumber]
    _ -> error $ "Instrument '" ++ unpack instrumentName ++ "' not found"

toInstrumentParameters :: [InstrumentJSON] -> Map Integer (Map Text Double)
toInstrumentParameters instrumentsJson = fromList $ map f instrumentsJson
  where f (InstrumentJSON number _ _ _ parameters _) = (number, parameters)
