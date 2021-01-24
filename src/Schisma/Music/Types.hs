module Schisma.Music.Types
  ( EnglishPitch(..)
  , EqualTemperament(..)
  , Pitch(..)
  )
where

data EnglishPitch
  = CDoubleFlat
  | CFlat
  | CNatural
  | DDoubleFlat
  | CSharp
  | DFlat
  | CDoubleSharp
  | DNatural
  | EDoubleFlat
  | DSharp
  | EFlat
  | FDoubleFlat
  | DDoubleSharp
  | ENatural
  | FFlat
  | ESharp
  | FNatural
  | GDoubleFlat
  | EDoubleSharp
  | FSharp
  | GFlat
  | FDoubleSharp
  | GNatural
  | ADoubleFlat
  | GSharp
  | AFlat
  | GDoubleSharp
  | ANatural
  | BDoubleFlat
  | ASharp
  | BFlat
  | ADoubleSharp
  | BNatural
  | BSharp
  | BDoubleSharp
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

data EqualTemperament = EqualTemperament deriving (Show)

data Pitch pitchName temperament = Pitch
  { tuningFrequency :: Double
  , tuningNote :: (pitchName, Integer)
  , temperament :: temperament
  , pitchName   :: pitchName
  , octave      :: Integer
  } deriving (Show)
