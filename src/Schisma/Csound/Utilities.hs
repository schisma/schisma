module Schisma.Csound.Utilities
  ( Passthrough(..)
  , debugk
  , extratime
  ) where

import           Data.List                      ( foldl' )
import           Data.Text                      ( Text )

import           Schisma.Csound.Opcodes.Display ( printkWithDefaults )
import           Schisma.Csound.Opcodes.MidiEventExtenders
                                                ( xtratim )
import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator )
import           Schisma.Csound.Types.Signals   ( ARateSignal(..)
                                                , IRateSignal(..)
                                                , IsSignal(getSignal)
                                                , KRateSignal(..)
                                                , Opcode(PassthroughOpcode)
                                                , Signal(Signal)
                                                , StatementOpcode
                                                )


class (SignalGenerator a) => Passthrough a where
  -- | Passes a signal through along with a StatementOpcode. Because
  --   StatementOpcodes produce no signals, this is used to maintain the signal
  --   flow graph.
  passthrough
    :: a               -- ^ @passthroughSignal@ - The signal to be passed
                       --   through.
    -> StatementOpcode -- ^ @statementOpcode@ - The statement opcode.
    -> a               -- ^ The returned signal, which is the same as the one
                       --   passed through.

instance Passthrough ARateSignal where
  passthrough passthroughSignal statementOpcode = ARateSignal signal   where
    opcode = PassthroughOpcode statementOpcode $ getSignal passthroughSignal
    signal = Signal opcode 1

instance Passthrough KRateSignal where
  passthrough passthroughSignal statementOpcode = KRateSignal signal   where
    opcode = PassthroughOpcode statementOpcode $ getSignal passthroughSignal
    signal = Signal opcode 1

instance Passthrough IRateSignal where
  passthrough passthroughSignal statementOpcode = IRateSignal signal   where
    opcode = PassthroughOpcode statementOpcode $ getSignal passthroughSignal
    signal = Signal opcode 1


-- | Debugs a k-rate signal at a specified interval.
debugk
  :: KRateSignal -- ^ @signal@ - The signal whose value should be printed.
  -> IRateSignal -- ^ @interval@ - Time in seconds between printings.
  -> KRateSignal -- ^ The returned signal, which is the same as the one being
                 --   printed.
debugk signal interval = passthrough signal printed
  where printed = printkWithDefaults interval signal

-- | Extends the duration of real-time generated events.
--
--   See 'Schisma.Csound.Opcodes.MidiEventExtenders.xtratim' for more
--   information.
extratime
  :: IRateSignal -- ^ @duration@ - The additional duration of the current
                 --   instrument instance.
  -> IRateSignal -- ^ The returned signal, which is just the @duration@.
extratime duration = passthrough duration (xtratim duration)
