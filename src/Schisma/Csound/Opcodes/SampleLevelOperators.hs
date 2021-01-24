module Schisma.Csound.Opcodes.SampleLevelOperators
  ( IRateConverter(..)
  , KRateConverter(..)
  , downsamp
  , downsampWithDefaults
  , fold
  , upsamp
  )
where

import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types

class (IsSignal a) => KRateConverter a where
  -- | Converts a i-rate parameter to an k-rate value. Or converts an
  --   a-rate value to a k-rate value by down-sampling.
  --
  --  <https://csound.com/docs/manual/opk.html Csound documentation>
  k
    :: a           -- ^ @input@ - The input signal.
    -> KRateSignal -- ^ The returned signal.
  k input = makeOpcodeSignal "k" [getSignal input]

instance KRateConverter ARateSignal
instance KRateConverter IRateSignal


class (IsSignal a) => IRateConverter a where
  -- | Returns an init-type equivalent of a k-rate argument, or directly
  --   returns an i-rate argument.
  --
  --  <https://csound.com/docs/manual/opi.html Csound documentation>
  i
    :: a           -- ^ @input@ - The input signal.
    -> IRateSignal -- ^ The returned signal.
  i input = makeOpcodeSignal "i" [getSignal input]

instance IRateConverter KRateSignal
instance IRateConverter IRateSignal


-- | Modify a signal by down-sampling.
--
--  'downsamp' converts an audio signal to a control signal by
--  downsampling. It produces one kval for each audio control period. The
--  optional window invokes a simple averaging process to suppress
--  foldover.
--
--  <https://csound.com/docs/manual/downsamp.html Csound documentation>
downsamp
  :: ARateSignal -- ^ @input@ - The input signal.
  -> IRateSignal -- ^ @windowLength@ - The window length in samples over which
                 --   the audio signal is averaged to determine
                 --   a downsampled value. Maximum length is ksmps; 0 and
                 --   1 imply no window averaging.
  -> KRateSignal -- ^ The returned signal.
downsamp input windowLength =
  makeOpcodeSignal "downsamp" [getSignal input, getSignal windowLength]

-- | 'downsampWithDefaults' is identical to 'downsamp' with a default value
--   supplied for @windowLength@ (@'i#' 0@).
--
--  <https://csound.com/docs/manual/downsamp.html Csound documentation>
downsampWithDefaults
  :: ARateSignal -- ^ @input@ - The input signal.
  -> KRateSignal -- ^ The returned signal.
downsampWithDefaults input =
  makeOpcodeSignal "downsamp" [getSignal input, getSignal (i# 0)]

-- | Adds artificial foldover to an audio signal.
--
--   'fold' is an opcode which creates artificial foldover. For
--   example, when @amount@ is equal to 1 with sr=44100, no foldover is added.
--   When @amount@ is set to 2, the foldover is equivalent to a downsampling
--   to 22050, when it is set to 4, to 11025 etc. Fractional values of
--   @amount@ are possible, allowing a continuous variation of foldover
--   amount. This can be used for a wide range of special effects.
--
--  <https://csound.com/docs/manual/fold.html Csound documentation>
fold
  :: ARateSignal -- ^ @input@ - The input signal.
  -> KRateSignal -- ^ @amount@ - The amount of foldover expressed in multiple
                 --   of sampling rate. Must be >= 1.
  -> ARateSignal -- ^ The returned signal.
fold input amount = makeOpcodeSignal "fold" [getSignal input, getSignal amount]

-- | Modify a signal by up-sampling.
--
--   'upsamp' converts a control signal to an audio signal. It does it by
--   simple repetition of the kval.
--
--  <https://csound.com/docs/manual/upsamp.html Csound documentation>
upsamp
  :: KRateSignal -- ^ @input@ - The input signal.
  -> ARateSignal -- ^ The returned signal.
upsamp input = makeOpcodeSignal "upsamp" [getSignal input]
