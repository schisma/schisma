module Schisma.Csound.Score
  ( alwaysOnIStatement
  , fZeroStatement
  , soundToIStatement
  ) where

import           Data.Map.Strict                ( Map
                                                , elems
                                                )
import           Data.Text                      ( Text )

import           Schisma.Utilities              ( merge )

import           Schisma.Csound.Types.Score     ( FunctionTableStatement
                                                  ( FunctionTableStatement
                                                  , functionTableActionTime
                                                  , functionTableGenRoutine
                                                  , functionTableGenRoutineParameters
                                                  , functionTableNumber
                                                  , functionTableSize
                                                  )
                                                , InstrumentStatement
                                                  ( InstrumentStatement
                                                  , instrumentDurationTime
                                                  , instrumentNumber
                                                  , instrumentParameters
                                                  , instrumentStartingTime
                                                  )
                                                , ScoreStatement(..)
                                                , Sound
                                                  ( soundDuration
                                                  , soundParameters
                                                  , soundStartTime
                                                  )
                                                )

-- | Creates an "i statement" that results in an instrument being always
--   on.
--
--   <https://csound.com/docs/manual/i.html Csound documentation>
alwaysOnIStatement
  :: Integer        -- ^ @number@ - The instrument number.
  -> ScoreStatement -- ^ The "i statement".
alwaysOnIStatement number = IStatement instrumentStatement where
  instrumentStatement = InstrumentStatement
    { instrumentNumber       = fromIntegral number
    , instrumentStartingTime = -1
    , instrumentDurationTime = 604800
    , instrumentParameters   = []
    }

-- | Creates an "f 0 statement", which may be used to create an
--   action time with no associated action. Such time markers are useful for
--   padding out a score section and for letting Csound run
--   from realtime events only (e.g., using only MIDI input without score
--   events).
--
--   <https://csound.com/docs/manual/f.html Csound documentation>
fZeroStatement
  :: Integer        -- ^ @seconds@ - The number of seconds Csound will run.
  -> ScoreStatement -- ^ The "f 0 statement".
fZeroStatement seconds = FStatement $ FunctionTableStatement
  { functionTableNumber               = 0
  , functionTableActionTime           = seconds
  , functionTableSize                 = 0
  , functionTableGenRoutine           = 0
  , functionTableGenRoutineParameters = []
  }

-- | Converts a Sound to an "i statement". Any @soundParameters@ within the
--   @sound@ will take precedence over the supplied instrument
--   @parameters@.
--
--   <https://csound.com/docs/manual/i.html Csound documentation>
soundToIStatement
  :: (Double, Map Text Double) -- ^ @(number, parameters)@ - The instrument
                               --   number and its default parameters.
  -> Sound                     -- ^ @sound@ - The sound.
  -> ScoreStatement            -- ^ The "i statement".
soundToIStatement (number, parameters) sound = IStatement instrumentStatement where
  instrumentParameters = merge parameters $ soundParameters sound
  instrumentStatement  = InstrumentStatement
    { instrumentNumber       = number
    , instrumentStartingTime = soundStartTime sound
    , instrumentDurationTime = soundDuration sound
    , instrumentParameters   = elems instrumentParameters
    }
