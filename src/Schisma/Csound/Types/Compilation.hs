module Schisma.Csound.Types.Compilation
  ( ConditionalBlock(..)
  , InstrumentLine(..)
  , InstrumentState(..)
  ) where

import           Data.Map.Strict                ( Map )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )

import           Schisma.Csound.Types.Signals   ( Opcode
                                                , Udo
                                                )

data InstrumentLine = InstrumentLine
  { instrumentExpressionOutputIds :: [Text]
  , instrumentExpressionBody      :: Text
  , instrumentExpressionId        :: Integer
  }
  deriving (Show, Ord, Eq)

data InstrumentState = InstrumentState
  { instrumentLines :: [InstrumentLine]
  , opcodeBindings  :: Map Opcode [Text]
  , includedOpcodes :: Set Text
  , customUdos      :: Set Udo
  }
  deriving (Show, Ord, Eq)

data ConditionalBlock = ConditionalBlock
  { conditionalLines           :: [InstrumentLine]
  , conditionalOpcodeOutputIds :: [Text]
  , conditionalIncludedOpcodes :: Set Text
  , conditionalCustomUdos      :: Set Udo
  }
