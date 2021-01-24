module Schisma.Tracker.Translator
  ( trackerToScore
  )
where

import           Data.Map.Strict                ( Map
                                                , (!)
                                                , empty
                                                )
import           Data.List                      ( foldl'
                                                , scanl'
                                                , zip3
                                                )
import           Data.Text                      ( Text )

import           Schisma.Utilities              ( listDifference
                                                , merge
                                                , mapWithIndex
                                                )

import           Schisma.Csound.Score           ( soundToIStatement )

import           Schisma.Csound.Types

import           Schisma.Tracker.Types

-- | Converts the @tracker@ and associated @trackerConfiguration@ to a set
--   of score statements.
trackerToScore
  :: Tracker                  -- ^ @tracker@ - The tracker.
  -> TrackerFileConfiguration -- ^ @trackerConfiguration@ - The tracker file
                              --   configuration.
  -> [ScoreStatement]         -- ^ The score statements.
trackerToScore tracker trackerConfiguration = score
 where
  cellMappers = trackerCellMappers trackerConfiguration
  instruments = trackerInstruments trackerConfiguration
  instrumentParameters = trackerInstrumentParameters trackerConfiguration
  lineConstraints = trackerLineConstraints trackerConfiguration

  instrumentSounds =
    trackerToInstrumentSounds cellMappers tracker lineConstraints

  toIStatements (number, sounds) =
    map (soundToIStatement (number, instrumentParameters ! floor number)) sounds

  score = concatMap toIStatements instrumentSounds

trackerToInstrumentSounds
  :: CellMappers -> Tracker -> (Integer, Integer) -> [(Double, [Sound])]
trackerToInstrumentSounds cellMappers (Tracker lineNumbers masterSettings instrumentTracks) lineConstraints
  = instrumentSounds
 where
  initialMasterState = MasterSettingsState { trackerTuning         = "a440"
                                           , trackerTemperament    = "12tet"
                                           , trackerBeatsPerMinute = 60
                                           , trackerLinesPerBeat   = 16
                                           }

  allStates =
    scanl' updateMasterSettingsState initialMasterState masterSettings
  masterStates = case allStates of
    []     -> []
    x : xs -> xs

  soloedTracks = filter trackerTrackIsSolo instrumentTracks
  mutedTracks  = filter trackerTrackIsMute instrumentTracks

  tracks       = if null soloedTracks
    then listDifference instrumentTracks mutedTracks
    else soloedTracks

  trackToInstrumentSounds (InstrumentTrack number _ _ notes) index =
    ( read $ show number ++ "." ++ show index
    , instrumentCellsToSounds cellMappers
                              lineNumbers
                              masterStates
                              notes
                              lineConstraints
    )

  instrumentSounds = mapWithIndex trackToInstrumentSounds tracks

instrumentCellsToSounds
  :: CellMappers
  -> [Integer]
  -> [MasterSettingsState]
  -> [NoteCell]
  -> (Integer, Integer)
  -> [Sound]
instrumentCellsToSounds cellMappers lineNumbers masterSettings instrumentCells lineConstraints
  = sounds
 where
  initialMasterState = head masterSettings

  initialSound =
    Sound { soundStartTime = 0, soundDuration = 0, soundParameters = empty }

  (startingLineNumber, endingLineNumber) = lineConstraints
  playbackState                          = TrackerPlaybackState
    { trackerPlaybackTimeElapsed              = 0
    , trackerPlaybackPriorSounds              = [initialSound]
    , trackerPlaybackPriorLineNumber          = startingLineNumber
    , trackerPlaybackPriorMasterSettingsState = initialMasterState
    , trackerPriorNoteConcluded               = True
    , trackerPriorPitch                       = Nothing
    }

  rows  = zip3 lineNumbers masterSettings instrumentCells
  rows' = filter
    (\(lineNumber, _, _) ->
      lineNumber >= startingLineNumber && lineNumber <= endingLineNumber
    )
    rows
  instrumentSounds = trackerPlaybackPriorSounds
    $ foldl' (translateRow cellMappers) playbackState rows'

  sounds = tail instrumentSounds

translateRow
  :: CellMappers
  -> TrackerPlaybackState
  -> (Integer, MasterSettingsState, NoteCell)
  -> TrackerPlaybackState
translateRow cellMappers playbackState (lineNumber, masterState, noteCell) =
  state
 where
  priorMasterState = trackerPlaybackPriorMasterSettingsState playbackState
  bpm              = trackerBeatsPerMinute priorMasterState
  linesPerBeat     = trackerLinesPerBeat priorMasterState
  secondsPerBeat   = 60 / bpm
  priorLineNumber  = trackerPlaybackPriorLineNumber playbackState
  priorSoundDuration =
    fromIntegral (lineNumber - priorLineNumber) / linesPerBeat * secondsPerBeat

  timeElapsed        = trackerPlaybackTimeElapsed playbackState
  startTime          = timeElapsed + priorSoundDuration

  tuning             = trackerTuning masterState
  temperament        = trackerTemperament masterState

  frequencyMapper    = cellFrequencyMapper cellMappers
  toFrequency        = frequencyMapper lineNumber tuning temperament

  renameParameters   = cellParameterRenamer cellMappers

  priorSounds        = trackerPlaybackPriorSounds playbackState
  previousSound      = last priorSounds

  priorNoteConcluded = trackerPriorNoteConcluded playbackState
  priorPitch         = trackerPriorPitch playbackState

  currentSound       = Sound { soundStartTime  = startTime
                             , soundDuration   = 0
                             , soundParameters = empty
                             }

  (currentPitch, noteConcluded, newSounds) = case noteCell of
    NoteOn pitch effects ->
      let frequency  = toFrequency pitch
          parameters = renameParameters (merge frequency effects)
      in  if priorNoteConcluded
            then
              ( Just pitch
              , False
              , [previousSound, currentSound { soundParameters = parameters }]
              )
            else
              ( Just pitch
              , False
              , [ previousSound { soundDuration = priorSoundDuration }
                , currentSound { soundParameters = parameters }
                ]
              )
    NoteOff -> if priorNoteConcluded
      then (Nothing, True, [previousSound])
      else
        (Nothing, True, [previousSound { soundDuration = priorSoundDuration }])
    NoteBlank effects -> if priorNoteConcluded
      then (priorPitch, True, [previousSound])
      else
        let
          priorEffects = soundParameters previousSound
          parameters   = case priorPitch of
            Just p -> renameParameters
              (merge priorEffects (merge (toFrequency p) effects))
            Nothing -> effects
        in
          ( priorPitch
          , False
          , [ previousSound { soundDuration = priorSoundDuration * (-1) }
            , currentSound { soundParameters = parameters }
            ]
          )

  sounds = init priorSounds ++ newSounds

  state  = playbackState { trackerPlaybackTimeElapsed = startTime
                         , trackerPlaybackPriorSounds = sounds
                         , trackerPlaybackPriorLineNumber = lineNumber
                         , trackerPlaybackPriorMasterSettingsState = masterState
                         , trackerPriorNoteConcluded = noteConcluded
                         , trackerPriorPitch = currentPitch
                         }

updateMasterSettingsState
  :: MasterSettingsState -> MasterCell -> MasterSettingsState
updateMasterSettingsState masterState masterCell =
  let updateState state setting = case setting of
        BeatsPerMinute x -> state { trackerBeatsPerMinute = x }
        LinesPerBeat   x -> state { trackerLinesPerBeat = x }
  in  case masterCell of
        (MasterSettings settings) -> foldl' updateState masterState settings
        MasterBlankSettings       -> masterState
