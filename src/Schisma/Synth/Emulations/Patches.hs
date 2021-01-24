module Schisma.Synth.Emulations.Patches
  ( patchFieldsToPFieldParameters
  )
where

import           Data.List                      ( foldl' )
import           Data.Map.Strict                ( Map
                                                , empty
                                                , foldlWithKey
                                                , fromList
                                                , size
                                                )
import           Data.Text                      ( Text )

import           Schisma.Csound.Opcodes.MidiConverters
                                                ( ampmidiWithDefaults
                                                , cpsmidi
                                                )
import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , pi#
                                                )
import           Schisma.Csound.Types

import           Schisma.Utilities              ( merge )

patchFieldsToPFieldParameters :: [Text] -> Map Text IRateSignal
patchFieldsToPFieldParameters = foldl' f empty where
  f patch key =
    let pField = fromIntegral $ 4 + size patch
    in  merge patch (fromList [(key, pi# pField)])
