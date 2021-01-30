module Schisma.Csound.Types.Score
  ( FunctionTableStatement(..)
  , InstrumentStatement(..)
  , ScoreStatement(..)
  , Sound(..)
  ) where

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )

data InstrumentStatement = InstrumentStatement
  { instrumentNumber       :: Double
  , instrumentStartingTime :: Double
  , instrumentDurationTime :: Double
  , instrumentParameters   :: [Double]
  }
  deriving Show

data FunctionTableStatement = FunctionTableStatement
  { functionTableNumber               :: Integer
  , functionTableActionTime           :: Integer
  , functionTableSize                 :: Integer
  , functionTableGenRoutine           :: Integer
  , functionTableGenRoutineParameters :: [Double]
  }
  deriving Show

data ScoreStatement
  = IStatement InstrumentStatement
  | FStatement FunctionTableStatement

data Sound = Sound
  { soundStartTime  :: Double
  , soundDuration   :: Double
  , soundParameters :: Map Text Double
  }
  deriving Show
