module Schisma.CLI.Synth
  ( synthParameters
  ) where

import           Data.Map.Strict                ( Map
                                                , fromList
                                                )

import           Data.Text                      ( Text )

import           Schisma.Synth.Parameters       ( profit
                                                , soundFont
                                                )

import           Schisma.Csound.Types
import           Schisma.Synth.Types


synthParameters :: Map Text [SynthParameter]
synthParameters = fromList [("Profit", profit), ("SoundFont", soundFont)]
