module Schisma.Csound.Opcodes.MathematicalFunctions
  ( MathematicalFunction(..)
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal(..)
                                                , IRateSignal(..)
                                                , IsSignal(..)
                                                , KRateSignal(..)
                                                , Opcode(IncludedOpcode)
                                                , Signal(Signal)
                                                , SignalRate(ARate, KRate, IRate)
                                                )

class (IsSignal a, SignalGenerator a) => MathematicalFunction a where
  -- | Returns the absolute value of @x@.
  --
  --   <https://csound.com/docs/manual/abs.html Csound documentation>
  abs#
    :: a -- ^ @x@ - The input signal.
    -> a -- ^ The returned signal.
  abs# x = makeOpcodeSignal "abs" [getSignal x]

  -- | Returns the integer part of @x@.
  --
  --   <https://csound.com/docs/manual/int.html Csound documentation>
  int#
    :: a -- ^ @x@ - The input signal.
    -> a -- ^ The returned signal.
  int# x = makeOpcodeSignal "int" [getSignal x]

  -- | Returns the natural log of @x@, where @x@ is > 0.
  --
  --   <https://csound.com/docs/manual/log.html Csound documentation>
  log#
    :: a -- ^ @x@ - The input signal.
    -> a -- ^ The returned signal.
  log# x = makeOpcodeSignal "log" [getSignal x]

  -- | Returns the integer nearest to @x@; if the fractional part of @x@ is
  --   exactly 0.5, the number is rounded up.
  round2#
    :: a -- ^ @x@ - The input signal.
    -> a -- ^ The returned signal.

instance MathematicalFunction ARateSignal where
  round2# x = ARateSignal signal where
    args   = [getSignal x]
    opcode = IncludedOpcode "round2" args [ARate]
    signal = Signal opcode 1

instance MathematicalFunction KRateSignal where
  round2# x = KRateSignal signal where
    args   = [getSignal x]
    opcode = IncludedOpcode "round2" args [KRate]
    signal = Signal opcode 1

instance MathematicalFunction IRateSignal where
  round2# x = IRateSignal signal where
    args   = [getSignal x]
    opcode = IncludedOpcode "round2" args [IRate]
    signal = Signal opcode 1
