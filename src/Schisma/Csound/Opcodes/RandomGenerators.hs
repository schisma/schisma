module Schisma.Csound.Opcodes.RandomGenerators
  ( DistributionRandomGenerator(..)
  , NoiseGenerator(..)
  , RandomGenerator(..)
  , pinker
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

class (SignalGenerator a) => DistributionRandomGenerator a where
  -- | Cauchy distribution random number generator. This is an x-class noise
  --   generator.
  --
  --   Outputs both positive and negative numbers.
  --
  --   <https://csound.com/docs/manual/cauchy.html Csound documentation>
  cauchy
    :: KRateSignal -- ^ @alpha@ - Controls the spread from zero (the higher the
                   --   @alpha@, the larger the spread).
    -> a           -- ^ The returned signal.
  cauchy alpha = makeOpcodeSignal "cauchy" [getSignal alpha]

  -- | Linear distribution random number generator (positive values only). This
  --   is an x-class noise generator.
  --
  --   The range of the random numbers spans from 0 to @upperRange@.
  --
  --   <https://csound.com/docs/manual/linrand.html Csound documentation>
  linrand
    :: KRateSignal -- ^ @upperRange@ - The upper range of the random numbers.
    -> a           -- ^ The returned signal.
  linrand upperRange = makeOpcodeSignal "linrand" [getSignal upperRange]

  -- | Triangular distribution random number generator. This is an x-class
  --   noise generator.
  --
  --   The range of the random numbers spans from @-bound@ to @+bound@.
  --
  --   <https://csound.com/docs/manual/trirand.html Csound documentation>
  trirand
    :: KRateSignal -- ^ @bound@ - The range of the random numbers
                   --   (@-bound@ to @+bound@).
    -> a           -- ^ The returned signal.
  trirand bound = makeOpcodeSignal "trirand" [getSignal bound]

instance DistributionRandomGenerator ARateSignal
instance DistributionRandomGenerator KRateSignal
instance DistributionRandomGenerator IRateSignal


class (IsSignal a, SignalGenerator b) => RandomGenerator a b where
  -- | Generates a controlled pseudo-random number series between min and max
  --   values.
  --
  --   The 'random' opcode is similar to 'linrand' and 'trirand' but allows the
  --   user to set arbitrary minimum and maximum values.
  --
  --   <https://csound.com/docs/manual/random.html Csound documentation>
  random
    :: a -- ^ @minValue@ - The minimum range limit.
    -> a -- ^ @maxValue@ - The maximum range limit.
    -> b -- ^ The returned signal.
  random minValue maxValue =
    makeOpcodeSignal "random" $ map getSignal [minValue, maxValue]

instance RandomGenerator KRateSignal ARateSignal
instance RandomGenerator KRateSignal KRateSignal
instance RandomGenerator IRateSignal IRateSignal


class (IsSignal a) => NoiseGenerator a where
  -- | A white noise generator with an IIR lowpass filter.
  --
  --   The higher the @beta@, the lower the filter's cut-off frequency.
  --   The cutoff frequency is roughly @sr * ((1 - beta) / 2)@.
  --
  --   See the Csound documentation for the filter equation.
  --
  --   <https://csound.com/docs/manual/noise.html Csound documentation>
  noise
    :: a           -- ^ @amplitude@ - The amplitude of the final output.
    -> KRateSignal -- ^ @beta@ - The beta of the lowpass filter. Should be in
                   --   the range of -1 to 1, exclusive of the end-points.
    -> ARateSignal -- ^ The returned signal.
  noise amplitude beta =
    makeOpcodeSignal "noise" [getSignal amplitude, getSignal beta]

instance NoiseGenerator ARateSignal
instance NoiseGenerator KRateSignal
instance NoiseGenerator IRateSignal


-- | Generates pink noise (-3dB\/oct response) by the /New Shade of Pink/
--   algorithm of Stefan Stenzel.
--
--   'pinker' generates pink noise (i.e., noise with equal energy in each
--   octave). For details of the algorithm look at
--   http://stenzel.waldorfmusic.de/post/pink/.
--
--   <https://csound.com/docs/manual/pinker.html Csound documentation>
pinker :: ARateSignal -- ^ The returned signal.
pinker = makeOpcodeSignal "pinker" []
