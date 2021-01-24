module Schisma.Csound.Opcodes.SignalLimiters
  ( Limiter(..)
  )
where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types

class (IsSignal a, IsSignal b, SignalGenerator c) => Limiter a b c where
  -- | Sets the lower and upper limits of the value it processes.
  --
  --   If @high@ is lower than @low@, then the output will be the average of
  --   the two - it will not be affected by @input@.
  --
  --   <https://csound.com/docs/manual/limit.html Csound documentation>
  limit
    :: a -- ^ @input@ - The input signal.
    -> b -- ^ @low@ - The low threshold.
    -> b -- ^ @high@ - The high threshold.
    -> c -- ^ The returned signal.
  limit input low high = makeOpcodeSignal "limit" args where
    args =
      [ getSignal input
      , getSignal low
      , getSignal high
      ]

instance Limiter ARateSignal KRateSignal ARateSignal
instance Limiter KRateSignal KRateSignal KRateSignal
instance Limiter IRateSignal IRateSignal IRateSignal
