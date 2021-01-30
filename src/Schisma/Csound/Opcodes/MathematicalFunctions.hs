module Schisma.Csound.Opcodes.MathematicalFunctions
  ( MathematicalFunction(..)
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , IRateSignal
                                                , IsSignal(..)
                                                , KRateSignal
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

instance MathematicalFunction ARateSignal
instance MathematicalFunction KRateSignal
instance MathematicalFunction IRateSignal
