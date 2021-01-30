module Schisma.Csound.Opcodes.Filters
  ( BiquadFilter(..)
  , ZdfFilter(..)
  , atone
  , atoneWithDefaults
  , tone
  , toneWithDefaults
  , tonek
  , tonekWithDefaults
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , IRateSignal
                                                , IsSignal(..)
                                                , KRateSignal
                                                )

class (IsSignal a) => BiquadFilter a where
  -- | A sweepable general purpose biquadratic digital filter.
  --
  --   'biquad' is a general purpose biquadratic digital filter of the form:
  --
  --     a0*y(n) + a1*y[n-1] + a2*y[n-2] = b0*x[n] + b1*x[n-1] + b2*x[n-2]
  --
  --   This filter has the following frequency response:
  --
  --          B(Z)   b0 + b1*Z-1  + b2*Z-2
  --   H(Z) = ---- = ---------------------
  --          A(Z)   a0 + a1*Z-1  + a2*Z-2
  --
  --   This type of filter is often encountered in digital signal
  --   processing literature. It allows six user-defined coefficients.
  --
  --   <https://csound.com/docs/manual/biquad.html Csound documentation>
  biquad
    :: ARateSignal -- ^ @input@ - The input signal.
    -> a           -- ^ @b0@ - The coefficient b0.
    -> a           -- ^ @b1@ - The coefficient b1.
    -> a           -- ^ @b2@ - The coefficient b2.
    -> a           -- ^ @a0@ - The coefficient a0.
    -> a           -- ^ @a1@ - The coefficient a1.
    -> a           -- ^ @a2@ - The coefficient a2.
    -> IRateSignal -- ^ @skipInit@ - Should initialization be skipped?
    -> ARateSignal -- ^ The returned signal.

  -- | 'biquadWithDefaults' is identical to 'biquad' with a default value
  --   supplied for @skipInit@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/biquad.html Csound documentation>
  biquadWithDefaults
    :: ARateSignal -- ^ @input@ - The input signal.
    -> a           -- ^ @b0@ - The coefficient b0.
    -> a           -- ^ @b1@ - The coefficient b1.
    -> a           -- ^ @b2@ - The coefficient b2.
    -> a           -- ^ @a0@ - The coefficient a0.
    -> a           -- ^ @a1@ - The coefficient a1.
    -> a           -- ^ @a2@ - The coefficient a2.
    -> ARateSignal -- ^ The returned signal.

instance BiquadFilter ARateSignal where
  biquad input b0 b1 b2 a0 a1 a2 skipInit = makeOpcodeSignal "biquada" args   where
    args =
      [ getSignal input
      , getSignal b0
      , getSignal b1
      , getSignal b2
      , getSignal a0
      , getSignal a1
      , getSignal a2
      , getSignal skipInit
      ]

  biquadWithDefaults input b0 b1 b2 a0 a1 a2 = makeOpcodeSignal "biquada" args   where
    args =
      [ getSignal input
      , getSignal b0
      , getSignal b1
      , getSignal b2
      , getSignal a0
      , getSignal a1
      , getSignal a2
      , getSignal (i# 0)
      ]

instance BiquadFilter KRateSignal where
  biquad input b0 b1 b2 a0 a1 a2 skipInit = makeOpcodeSignal "biquad" args   where
    args =
      [ getSignal input
      , getSignal b0
      , getSignal b1
      , getSignal b2
      , getSignal a0
      , getSignal a1
      , getSignal a2
      , getSignal skipInit
      ]

  biquadWithDefaults input b0 b1 b2 a0 a1 a2 = makeOpcodeSignal "biquad" args   where
    args =
      [ getSignal input
      , getSignal b0
      , getSignal b1
      , getSignal b2
      , getSignal a0
      , getSignal a1
      , getSignal a2
      , getSignal (i# 0)
      ]


class (IsSignal a) => ZdfFilter a where
  -- | Zero-delay feedback implementation of a 4 pole (24 dB/oct) low-pass
  --   filter based on the Moog ladder filter.
  --
  --   <https://csound.com/docs/manual/zdf_ladder.html Csound documentation>
  zdfLadder
    :: ARateSignal -- ^ @input@ - The input signal.
    -> a           -- ^ @cutoffFrequency@ - The filter cutoff frequency.
    -> a           -- ^ @q@ - The filter Q value. Should be in the range of
                   --   0.5-25.0. Self-oscillation occurs at 25.0.
    -> IRateSignal -- ^ @disposition@ - The initial disposition of internal
                   --   data space. Since filtering incorporates a feedback
                   --   loop of previous output, the initial status of the
                   --   storage space used is significant. A zero value will
                   --   clear the space; a non-zero value will allow previous
                   --   information to remain.
    -> ARateSignal -- ^ The returned signal.
  zdfLadder input cutoffFrequency q disposition =
    makeOpcodeSignal "zdf_ladder" args where
      args =
        [getSignal input
        , getSignal cutoffFrequency
        , getSignal q
        , getSignal disposition
        ]

  -- | 'zdfLadderWithDefaults' is identical to 'zdfLadder' with a default value
  --   supplied for @disposition@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/zdf_ladder.html Csound documentation>
  zdfLadderWithDefaults
    :: ARateSignal -- ^ @input@ - The input signal.
    -> a           -- ^ @cutoffFrequency@ - The filter cutoff frequency.
    -> a           -- ^ @q@ - The filter Q value. Should be in the range of
                   --   0.5-25.0. Self-oscillation occurs at 25.0.
    -> ARateSignal -- ^ The returned signal.
  zdfLadderWithDefaults input cutoffFrequency q =
    zdfLadder input cutoffFrequency q (i# 0)

instance ZdfFilter ARateSignal
instance ZdfFilter KRateSignal
instance ZdfFilter IRateSignal


-- | A hi-pass filter whose transfer functions are the complements of the
--   'tone' opcode.
--
--   'atone' is a filter whose transfer functions is the complement of 'tone'.
--   'atone' is thus a form of high-pass filter whose transfer functions
--   represent the "filtered out" aspects of their complements. However,
--   power scaling is not normalized in 'atone' but remains the true complement
--   of the corresponding unit. Thus an audio signal, filtered by parallel
--   matching 'tone' and 'atone' units, would under addition simply reconstruct
--   the original spectrum.
--
--   This property is particularly useful for controlled mixing of
--   different sources (see 'lpreson'). Complex response curves such as those
--   with multiple peaks can be obtained by using a bank of suitable
--   filters in series. (The resultant response is the product of the
--   component responses.) In such cases, the combined attenuation may
--   result in a serious loss of signal power, but this can be regained by
--   the use of 'balance'.
--
--   <https://csound.com/docs/manual/atone.html Csound documentation>
atone
  :: ARateSignal -- ^ @input@ - The input signal.
  -> KRateSignal -- ^ @halfPowerPoint@ - The response curve's half-power point,
                 --   in Hertz. Half power is defined as peak power / root 2.
  -> IRateSignal -- ^ @disposition@ - The initial disposition of internal data
                 --   space. Since filtering incorporates a feedback loop of
                 --   previous output, the initial status of the storage space
                 --   used is significant. A zero value will clear the space;
                 --   a non-zero value will allow previous information to
                 --   remain.
  -> ARateSignal -- ^ The returned signal.
atone input halfPowerPoint disposition = makeOpcodeSignal
  "atone"
  [getSignal input, getSignal halfPowerPoint, getSignal disposition]

-- | 'atoneWithDefaults' is identical to 'atone' with a default value supplied
--   for @disposition@ (@'i#' 0@).
--
--   <https://csound.com/docs/manual/atone.html Csound documentation>
atoneWithDefaults
  :: ARateSignal -- ^ @input@ - The input signal.
  -> KRateSignal -- ^ @halfPowerPoint@ - The response curve's half-power point,
                 --   in Hertz. Half power is defined as peak power / root 2.
  -> ARateSignal -- ^ The returned signal.
atoneWithDefaults input halfPowerPoint = makeOpcodeSignal
  "atone"
  [getSignal input, getSignal halfPowerPoint, getSignal (i# 0)]

-- | A first-order recursive low-pass filter with variable frequency response.
--
--   'tone' is a 1 term IIR filter. Its formula is:
--    yn = c1 * xn + c2 * yn-1
--
--    where
--      b = 2 - cos(2π hp/sr);
--      c2 = b - sqrt(b2 - 1.0)
--      c1 = 1 - c2
--
--   <https://csound.com/docs/manual/tone.html Csound documentation>
tone
  :: ARateSignal -- ^ @input@ - The input signal.
  -> KRateSignal -- ^ @halfPowerPoint@ - The response curve's half-power point,
                 --   in Hertz. Half power is defined as peak power / root 2.
  -> IRateSignal -- ^ @disposition@ - The initial disposition of internal data
                 --   space. Since filtering incorporates a feedback loop of
                 --   previous output, the initial status of the storage space
                 --   used is significant. A zero value will clear the space;
                 --   a non-zero value will allow previous information to
                 --   remain.
  -> ARateSignal -- ^ The returned signal.
tone input halfPowerPoint disposition = makeOpcodeSignal
  "tone"
  [getSignal input, getSignal halfPowerPoint, getSignal disposition]

-- | 'toneWithDefaults' is identical to 'tone' with a default value supplied
--   for @disposition@ (@'i#' 0@).
--
--   <https://csound.com/docs/manual/tone.html Csound documentation>
toneWithDefaults
  :: ARateSignal -- ^ @input@ - The input signal.
  -> KRateSignal -- ^ @halfPowerPoint@ - The response curve's half-power point,
                 --   in Hertz. Half power is defined as peak power / root 2.
  -> ARateSignal -- ^ The returned signal.
toneWithDefaults input halfPowerPoint = makeOpcodeSignal
  "tone"
  [getSignal input, getSignal halfPowerPoint, getSignal (i# 0)]

-- | A first-order recursive low-pass filter with variable frequency response.
--
--   'tonek' is like 'tone' except its output is at control rate rather than
--   audio rate.
--
--   <https://csound.com/docs/manual/tonek.html Csound documentation>
tonek
  :: KRateSignal -- ^ @input@ - The input signal.
  -> KRateSignal -- ^ @halfPowerPoint@ - The response curve's half-power point,
                 --   in Hertz. Half power is defined as peak power / root 2.
  -> IRateSignal -- ^ @disposition@ - The initial disposition of internal data
                 --   space. Since filtering incorporates a feedback loop of
                 --   previous output, the initial status of the storage space
                 --   used is significant. A zero value will clear the space;
                 --   a non-zero value will allow previous information to
                 --   remain.
  -> KRateSignal -- ^ The returned signal.
tonek input halfPowerPoint disposition = makeOpcodeSignal
  "tonek"
  [getSignal input, getSignal halfPowerPoint, getSignal disposition]

-- | 'tonekWithDefaults' is identical to 'tonek' with a default value supplied
--   for @disposition@ (@'i#' 0@).
--
--   <https://csound.com/docs/manual/tonek.html Csound documentation>
tonekWithDefaults
  :: KRateSignal -- ^ @input@ - The input signal.
  -> KRateSignal -- ^ @halfPowerPoint@ - The response curve's half-power point,
                 --   in Hertz. Half power is defined as peak power / root 2.
  -> KRateSignal -- ^ The returned signal.
tonekWithDefaults input halfPowerPoint = makeOpcodeSignal
  "tonek"
  [getSignal input, getSignal halfPowerPoint, getSignal (i# 0)]
