module Main where

import           Prelude                 hiding ( concat
                                                , putStrLn
                                                )


import           Options.Applicative            ( (<**>)
                                                , Parser
                                                , command
                                                , execParser
                                                , fullDesc
                                                , header
                                                , helper
                                                , hsubparser
                                                , info
                                                , progDesc
                                                )

import           Data.ByteString.Lazy.Char8     ( putStrLn )

import           Data.Aeson                     ( eitherDecodeFileStrict'
                                                , encode
                                                )

import           Data.Map.Strict                ( empty )
import           Data.Maybe                     ( fromMaybe )


import           Schisma.Csound.Types.Instruments
                                                ( Instrument(instrumentNumber) )


import           Schisma.Tracker.Mappers        ( defaultFrequencyMapper
                                                , frequencyWithRandomDetuningMapper
                                                , twelveTetA440ToFrequency
                                                )
import           Schisma.Tracker.Types          ( CellMappers(..)
                                                , TrackerFileConfiguration(..)
                                                )
import           Schisma.Utilities              ( renameKeysFromMap )

import           Schisma.CLI.Synth              ( synths )
import           Schisma.CLI.Tracker            ( CompositionFileJSON(..)
                                                , InstrumentsFileJSON(..)
                                                , PlayTrackerOptions(..)
                                                , ProjectFileJSON(..)
                                                , TrackerSettingsJSON(..)
                                                , playTrackerOptionsParser
                                                , toInstrumentParameters
                                                , toInstruments
                                                )
import           Schisma.CLI.TrackerSettings    ( toFrequencyMapper )
import           Schisma.IO                     ( playTrackerFile )

-- TODO: Doc

data Command
  = Tracker TrackerCommand
  | Synth SynthCommand
  deriving (Show)

newtype TrackerCommand = Play PlayTrackerOptions
  deriving Show

data SynthCommand = SynthList
  deriving Show


main :: IO ()
main = run =<< execParser opts
 where
  opts = info
    (parser <**> helper)
    (fullDesc <> progDesc "A lightweight composition tool." <> header "schisma")

run :: Command -> IO ()
run (Tracker trackerCommand) = case trackerCommand of
  (Play options) -> playTracker options
run (Synth synthCommand) = case synthCommand of
  SynthList -> printSynthParameters

parser :: Parser Command
parser = hsubparser
  (  command "tracker" (info trackerParser (progDesc "Work with trackers"))
  <> command "synth"   (info synthParser (progDesc "Work with synths"))
  )

synthParser :: Parser Command
synthParser = Synth <$> hsubparser
  (command
    "list"
    (info (pure SynthList) (progDesc "Returns the list of available synths"))
  )

trackerParser :: Parser Command
trackerParser = Tracker <$> hsubparser
  (command
    "play"
    (info (Play <$> playTrackerOptionsParser)
          (progDesc "Compiles and runs the supplied tracker file")
    )
  )

printSynthParameters :: IO ()
printSynthParameters = putStrLn $ encode synths

playTracker :: PlayTrackerOptions -> IO ()
playTracker (PlayTrackerOptions projectFile startingLine endingLine) = do
  projectFileConfig <-
    eitherDecodeFileStrict' projectFile :: IO (Either String ProjectFileJSON)
  let projectConfig = case projectFileConfig of
        Left  message -> error ("Invalid project file. " ++ message)
        Right json    -> json

  instrumentsFileConfig <-
    eitherDecodeFileStrict' (instrumentsFile projectConfig) :: IO
      (Either String InstrumentsFileJSON)
  let instrumentsConfig = case instrumentsFileConfig of
        Left  message -> error ("Invalid instruments file. " ++ message)
        Right json    -> json

  compositionFileConfig <-
    eitherDecodeFileStrict' (compositionFile projectConfig) :: IO
      (Either String CompositionFileJSON)
  let compositionConfig = case compositionFileConfig of
        Left  message -> error ("Invalid composition file. " ++ message)
        Right json    -> json

  let trackerConfig = trackerSettings compositionConfig

  let cellMappers = CellMappers
      -- TODO: The frequency mapper should have a track number
      -- associated with it so that it can be applied differently across
      -- tracks
        { cellFrequencyMapper  = toFrequencyMapper
                                   (frequencyMapper trackerConfig)
        , cellParameterRenamer = renameKeysFromMap
                                   (parameterRenamings trackerConfig)
        }

  let instrumentsJson    = instruments instrumentsConfig
  let trackerInstruments = concatMap toInstruments instrumentsJson

  let trackerFileConfiguration = TrackerFileConfiguration
        cellMappers
        trackerInstruments
        (toInstrumentParameters instrumentsJson)
        (startingLine, endingLine)


  -- TODO: Expose headerStatements, don't use empty
  playTrackerFile Data.Map.Strict.empty
                  (trackerFile projectConfig)
                  trackerFileConfiguration
