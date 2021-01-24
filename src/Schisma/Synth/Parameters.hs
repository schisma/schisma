module Schisma.Synth.Parameters
  ( profit
  , soundFont
  ) where

import           Data.Map.Strict                ( Map )

import           Data.Text                      ( Text )

import qualified Schisma.Synth.SoundFont as SoundFont
                                                ( synthParameters )

import qualified Schisma.Synth.Emulations.Profit
                                               as Profit
                                                ( synthParameters )

import           Schisma.Csound.Types
import           Schisma.Synth.Types

-- | Retrieves the parameters of the Profit synthesizer.
profit :: [SynthParameter] -- ^ The synth parameters.
profit = Profit.synthParameters

-- | Retrieves the parameters of the SoundFont synthesizer.
soundFont :: [SynthParameter] -- ^ The synth parameters.
soundFont = SoundFont.synthParameters
