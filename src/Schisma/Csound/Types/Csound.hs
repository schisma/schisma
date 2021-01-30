module Schisma.Csound.Types.Csound
  ( Csd(..)
  ) where

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )

import           Schisma.Csound.Types.Instruments
                                                ( Instrument )
import           Schisma.Csound.Types.Score     ( ScoreStatement )

data Csd = Csd
  { csdOptions                   :: Text
  , csdOrchestraHeaderStatements :: Map Text Text
  , csdInstruments               :: [Instrument]
  , csdScore                     :: [ScoreStatement]
  }
