module Schisma.Music.Translator
  ( Tuning(pitchToFrequency)
  , toEnglishPitch
  )
where

import           Data.Text                      ( Text )

import           Schisma.Music.Types

class Tuning pitchName temperament where
  -- | Converts a pitch to a frequency.
  --
  --   <https://www.dolmetsch.com/musictheory27.htm#calculator Frequency calculator>
  pitchToFrequency
    :: Pitch pitchName temperament -- ^ @pitch@ - The pitch.
    -> Double                      -- ^ The frequency.

instance Tuning EnglishPitch EqualTemperament where
  pitchToFrequency pitch = tuningFrequency pitch * (2 ** exponent)   where
    pitchNameToInteger name = case name of
      CDoubleFlat  -> -2
      CFlat        -> -1
      CNatural     -> 0
      DDoubleFlat  -> 0
      CSharp       -> 1
      DFlat        -> 1
      CDoubleSharp -> 2
      DNatural     -> 2
      EDoubleFlat  -> 2
      DSharp       -> 3
      EFlat        -> 3
      FDoubleFlat  -> 3
      DDoubleSharp -> 4
      ENatural     -> 4
      FFlat        -> 4
      ESharp       -> 5
      FNatural     -> 5
      GDoubleFlat  -> 5
      EDoubleSharp -> 6
      FSharp       -> 6
      GFlat        -> 6
      FDoubleSharp -> 7
      GNatural     -> 7
      ADoubleFlat  -> 7
      GSharp       -> 8
      AFlat        -> 8
      GDoubleSharp -> 9
      ANatural     -> 9
      BDoubleFlat  -> 9
      ASharp       -> 10
      BFlat        -> 10
      ADoubleSharp -> 11
      BNatural     -> 11
      BSharp       -> 12
      BDoubleSharp -> 13

    pitchNumber (name, octave) = 12 * (octave + 1) + pitchNameToInteger name

    tuningNumber = pitchNumber $ tuningNote pitch
    noteNumber   = pitchNumber (pitchName pitch, octave pitch)
    distance     = noteNumber - tuningNumber
    exponent     = fromIntegral distance / 12


-- | Converts a note name to an EnglishPitch.
toEnglishPitch
  :: Text         -- ^ @noteName@ - The note name.
  -> EnglishPitch -- ^ The EnglishPitch.
toEnglishPitch noteName = case noteName of
  "Cbb" -> CDoubleFlat
  "Dbb" -> DDoubleFlat
  "Ebb" -> EDoubleFlat
  "Fbb" -> FDoubleFlat
  "Gbb" -> GDoubleFlat
  "Abb" -> ADoubleFlat
  "Bbb" -> BDoubleFlat
  "Cb"  -> CFlat
  "Db"  -> DFlat
  "Eb"  -> EFlat
  "Fb"  -> FFlat
  "Gb"  -> GFlat
  "Ab"  -> AFlat
  "Bb"  -> BFlat
  "C##" -> CDoubleSharp
  "D##" -> DDoubleSharp
  "E##" -> EDoubleSharp
  "F##" -> FDoubleSharp
  "G##" -> GDoubleSharp
  "A##" -> ADoubleSharp
  "B##" -> BDoubleSharp
  "C#"  -> CSharp
  "D#"  -> DSharp
  "E#"  -> ESharp
  "F#"  -> FSharp
  "G#"  -> GSharp
  "A#"  -> ASharp
  "B#"  -> BSharp
  "C"   -> CNatural
  "D"   -> DNatural
  "E"   -> ENatural
  "F"   -> FNatural
  "G"   -> GNatural
  "A"   -> ANatural
  "B"   -> BNatural
