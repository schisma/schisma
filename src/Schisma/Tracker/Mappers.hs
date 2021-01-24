module Schisma.Tracker.Mappers
  ( defaultFrequencyMapper
  , frequencyWithRandomDetuningMapper
  , twelveTetA440ToFrequency
  ) where

import           System.Random                  ( mkStdGen
                                                , randomR
                                                )

import           Data.Char                      ( isDigit )
import           Data.List                      ( partition )
import           Data.Map.Strict                ( Map
                                                , singleton
                                                )
import           Data.Text                      ( Text
                                                , partition
                                                , unpack
                                                )

import           Schisma.Music.Translator       ( toEnglishPitch
                                                , pitchToFrequency
                                                )
import           Schisma.Music.Types

-- A convenience function for common frequency mappers.
defaultFrequencyMapper
  :: Integer         -- ^ @lineNumber@ - The current line number.
  -> Text            -- ^ @tuning@ - The tuning pitch.
  -> Text            -- ^ @temperament@ - The temperament.
  -> Text            -- ^ @note@ - The note.
  -> Map Text Double -- ^ A map containing a "frequency" key along with the
                     --   note translated to a frequency.
defaultFrequencyMapper lineNumber tuning temperament note = result
 where
  frequency = if tuning == "a440" && temperament == "12tet"
    then twelveTetA440ToFrequency note
    else 0

  result = singleton "frequency" frequency

-- A frequency mapper that supports random detuning.
frequencyWithRandomDetuningMapper
  :: Double          -- ^ @limit@ - The maximum deviation of cents.
  -> Integer         -- ^ @lineNumber@ - The current line number.
  -> Text            -- ^ @tuning@ - The tuning pitch.
  -> Text            -- ^ @temperament@ - The temperament.
  -> Text            -- ^ @note@ - The note.
  -> Map Text Double -- ^ A map containing a "frequency" key along with the
                     --   note translated to a frequency.
frequencyWithRandomDetuningMapper limit lineNumber tuning temperament note =
  result where
  frequency = if tuning == "a440" && temperament == "12tet"
    then twelveTetA440ToFrequency note
    else 0

  generator  = mkStdGen $ fromIntegral lineNumber
  (cents, _) = randomR (negate limit, limit) generator
  detuned    = (2.0 ** (cents / 1200.0)) * frequency

  result     = singleton "frequency" detuned

-- Converts a note from 12-TET (using A440 as the tuning pitch) to
-- a frequency.
--
-- ==== __Examples__
--
-- >>> twelveTetA440ToFrequency "A2"
-- 110.0
twelveTetA440ToFrequency
  :: Text   -- ^ @note@ - The note.
  -> Double -- ^ The frequency.
twelveTetA440ToFrequency note = pitchToFrequency pitch where
  (noteName, octave) = Data.Text.partition (not . isDigit) note
  pitch              = Pitch { tuningFrequency = 440
                             , tuningNote      = (ANatural, 4)
                             , temperament     = EqualTemperament
                             , pitchName       = toEnglishPitch noteName
                             , octave          = read (unpack octave)
                             }
