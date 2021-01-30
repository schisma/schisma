module Schisma.Csound.Types.Instruments
  ( Instrument(..)
  ) where


import           Schisma.Csound.Types.Signals   ( Opcode )

data Instrument = Instrument Opcode Integer
  deriving (Show, Ord, Eq)
