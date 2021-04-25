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
                                                ( Instrument(Instrument) )


import           Schisma.Tracker.Mappers        ( defaultFrequencyMapper
                                                , frequencyWithRandomDetuningMapper
                                                , twelveTetA440ToFrequency
                                                )
import           Schisma.Tracker.Types          ( CellMappers(..)
                                                , TrackerFileConfiguration(..)
                                                )
import           Schisma.Utilities              ( renameKeysFromMap )

import           Schisma.CLI.Synth              ( synths )
import           Schisma.CLI.Tracker            ( PlayTrackerOptions(..)
                                                , TrackerJSON(..)
                                                , playTrackerOptionsParser
                                                , toInstrumentParameters
                                                , toInstruments
                                                )
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
run (Tracker trackerCommand) = do
  case trackerCommand of
    (Play options) -> playTracker options
run (Synth synthCommand) = do
  case synthCommand of
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
    (info (pure SynthList)
          (progDesc "Returns the list of available synths")
    )
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
printSynthParameters = do
  putStrLn $ encode synths

playTracker :: PlayTrackerOptions -> IO ()
playTracker (PlayTrackerOptions trackerFile instrumentFile startingLine endingLine)
  = do
    instrumentConfig <-
      eitherDecodeFileStrict' instrumentFile :: IO (Either String TrackerJSON)
    let config = case instrumentConfig of
          Left  message -> error ("Invalid instrument file. " ++ message)
          Right json    -> json

    let cellMappers = CellMappers
        -- TODO: Expose frequency mapper
        -- TODO: The frequency mapper should have a track number
        -- associated with it so that it can be applied differently across
        -- tracks
          { cellFrequencyMapper  = defaultFrequencyMapper
          --{ cellFrequencyMapper  = frequencyWithRandomDetuningMapper 2.5
          , cellParameterRenamer = renameKeysFromMap (parameterRenamings config)
          }

    let instrumentsJson     = instruments config

    let trackerInstruments  = map toInstruments instrumentsJson
    let normalInstruments   = concatMap fst trackerInstruments
    let alwaysOnInstruments = concatMap snd trackerInstruments
    let allInstruments      = normalInstruments ++ alwaysOnInstruments

    let toInstrumentNumber (Instrument _ number) = number

    let trackerFileConfiguration = TrackerFileConfiguration
          cellMappers
          allInstruments
          (toInstrumentParameters instrumentsJson)
          (startingLine, endingLine)


    -- TODO: Expose headerStatements, don't use empty
    playTrackerFile Data.Map.Strict.empty
                    (map toInstrumentNumber alwaysOnInstruments)
                    trackerFile
                    trackerFileConfiguration
