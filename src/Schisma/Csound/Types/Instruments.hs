module Schisma.Csound.Types.Instruments
  ( Instrument(..)
  ) where


import           Schisma.Csound.Types.Signals   ( Opcode )

data Instrument = Instrument
  { instrumentOpcode :: Opcode
  , instrumentNumber :: Integer
  , instrumentAlwaysOn :: Bool
  }
  deriving (Show, Ord, Eq)
