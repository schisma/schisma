module Schisma.Csound.Opcodes.ExponentialGenerators
  ( ExponentialGenerator(..)
  , expcurve
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , IRateSignal
                                                , IsSignal(getSignal)
                                                , KRateSignal
                                                )

class (SignalGenerator a) => ExponentialGenerator a where
  -- | Trace an exponential curve between specified points.
  --
  --   These units generate control or audio signals whose values can pass
  --   through 2 specified points. The @duration@ value may or may not equal
  --   the instrument's performance time: a shorter performance will truncate
  --   the specified pattern, while a longer one will cause the defined
  --   segment to continue on in the same direction.
  --
  --   <https://csound.com/docs/manual/expon.html Csound documentation>
  expon
    :: IRateSignal -- ^ @start@ - The starting value. Zero is illegal for
                   --   exponentials.
    -> IRateSignal -- ^ @duration@ - The duration in seconds of the segment. A
                   --   zero or negative value will cause all initialization to
                   --   be skipped.
    -> IRateSignal -- ^ @end@ - The value after @duration@ seconds. For
                   --   exponentials, must be non-zero and must agree in
                   --   sign with @start@.
    -> a           -- ^ The returned signal.
  expon start duration end =
    makeOpcodeSignal "expon" $ map getSignal [start, duration, end]

instance ExponentialGenerator ARateSignal
instance ExponentialGenerator KRateSignal


-- | Generates an exponential curve in range 0 to 1 of arbitrary steepness.
--   Steepness index equal to or lower than 1.0 will result in
--   Not-a-Number errors and cause unstable behavior.
--
--   The formula used to calculate the curve is:
--   (exp(x * log(y))-1) / (y-1)
--
--   where x is equal to @index@ and y is equal to @steepness@.
--
--   <https://csound.com/docs/manual/expcurve.html Csound documentation>
expcurve
  :: KRateSignal    -- ^ @index@ - The index value. Expected range is 0 to 1.
  -> KRateSignal -- ^ @steepness@ - Steepness of the generated curve. Values
                 --   closer to 1.0 result in a straighter line while
                 --   larger values steepen the curve.
  -> KRateSignal -- ^ The returned signal.
expcurve index steepness =
  makeOpcodeSignal "expcurve" $ map getSignal [index, steepness]
