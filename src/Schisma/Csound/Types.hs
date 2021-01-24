module Schisma.Csound.Types
  ( ARateSignal(..)
  , Conditional(..)
  , ConditionalBlock(..)
  , ContainsSignals
  , Csd(..)
  , FunctionTableStatement(..)
  , IRateSignal(..)
  , KRateSignal(..)
  , IsSignal
  , Instrument(..)
  , InstrumentLine(..)
  , InstrumentState(..)
  , InstrumentStatement(..)
  , Opcode(..)
  , OrdinaryStatement(..)
  , SRateSignal(..)
  , ScoreStatement(..)
  , Signal(..)
  , SignalRate(..)
  , Sound(..)
  , StatementOpcode(..)
  , Udo(..)
  , getSignal
  , getSignals
  ) where

import           Data.Map.Strict                ( Map )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )

data Signal = Signal
  { outputOpcode :: Opcode
  , outputNumber :: Integer
  }
  deriving (Show, Ord, Eq)


newtype ARateSignal = ARateSignal Signal deriving (Show)
newtype KRateSignal = KRateSignal Signal deriving (Show)
newtype IRateSignal = IRateSignal Signal deriving (Show)
newtype SRateSignal = SRateSignal Signal deriving (Show)

class IsSignal a where
  getSignal :: a -> Signal

instance IsSignal ARateSignal where
  getSignal (ARateSignal signal) = signal

instance IsSignal KRateSignal where
  getSignal (KRateSignal signal) = signal

instance IsSignal IRateSignal where
  getSignal (IRateSignal signal) = signal

instance IsSignal SRateSignal where
  getSignal (SRateSignal signal) = signal


class ContainsSignals a where
  getSignals :: a -> [Signal]

instance ContainsSignals ARateSignal where
  getSignals (ARateSignal signal) = [signal]

instance ContainsSignals [ARateSignal] where
  getSignals []                   = []
  getSignals (ARateSignal x : xs) = x : getSignals xs

instance ContainsSignals KRateSignal where
  getSignals (KRateSignal signal) = [signal]

instance ContainsSignals [KRateSignal] where
  getSignals []                   = []
  getSignals (KRateSignal x : xs) = x : getSignals xs

instance ContainsSignals IRateSignal where
  getSignals (IRateSignal signal) = [signal]

instance ContainsSignals [IRateSignal] where
  getSignals []                   = []
  getSignals (IRateSignal x : xs) = x : getSignals xs

instance ContainsSignals SRateSignal where
  getSignals (SRateSignal signal) = [signal]

instance ContainsSignals [SRateSignal] where
  getSignals []                   = []
  getSignals (SRateSignal x : xs) = x : getSignals xs


data Conditional
  = Predicate Text (Signal, Signal)
  | CompoundPredicate Text (Conditional, Conditional)
  deriving (Show, Ord, Eq)

data StatementOpcode = StatementOpcode Text [Signal]
  deriving (Show, Ord, Eq)

data OrdinaryStatement
  = Op StatementOpcode
  | NoOp
  | ConditionalStatement Conditional (OrdinaryStatement, OrdinaryStatement)
  deriving (Show, Ord, Eq)

data Udo = Udo
  { udoName :: Text
  , udoInputRates :: [SignalRate]
  , udoOutputRates :: [SignalRate]
  , udoOpcode :: Opcode
  , udoControlPeriodSamples :: Integer
  } deriving (Show, Ord, Eq)

data SignalRate
  = ARate
  | IRate
  | KRate
  | SRate
  deriving (Show, Ord, Eq)

data Opcode
  = ConstDouble Double SignalRate
  | ConstInteger Integer SignalRate
  | ConstText Text SignalRate
  | PField Integer SignalRate
  | BinaryOperator Text (Signal, Signal) SignalRate
  | ConditionalExpression Conditional ([Signal], [Signal])
  | Opcode Text [Signal] [SignalRate]
  | IncludedOpcode Text [Signal] [SignalRate]
  | UserDefinedOpcode Udo [Signal]
  | MutatingOpcode Text [Signal] [Integer]
  | PassthroughOpcode StatementOpcode Signal
  | TerminalOpcode OrdinaryStatement
  deriving (Show, Ord, Eq)

data Instrument = Instrument Opcode Integer
  deriving (Show, Ord, Eq)

data InstrumentStatement = InstrumentStatement
  { instrumentNumber       :: Double
  , instrumentStartingTime :: Double
  , instrumentDurationTime :: Double
  , instrumentParameters   :: [Double]
  }
  deriving Show

data FunctionTableStatement = FunctionTableStatement
  { functionTableNumber               :: Integer
  , functionTableActionTime           :: Integer
  , functionTableSize                 :: Integer
  , functionTableGenRoutine           :: Integer
  , functionTableGenRoutineParameters :: [Double]
  }
  deriving Show

data ScoreStatement
  = IStatement InstrumentStatement
  | FStatement FunctionTableStatement

data Csd = Csd
  { csdOptions                   :: Text
  , csdOrchestraHeaderStatements :: Map Text Text
  , csdInstruments               :: [Instrument]
  , csdScore                     :: [ScoreStatement]
  }


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


data Sound = Sound
  { soundStartTime  :: Double
  , soundDuration   :: Double
  , soundParameters :: Map Text Double
  }
  deriving Show
