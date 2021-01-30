module Schisma.Csound.Opcodes.AmplitudeModifiers
  ( balance
  , balanceWithDefaults
  , compress2
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , IRateSignal
                                                , IsSignal(getSignal)
                                                , KRateSignal
                                                )

-- | Adjust one audio signal according to the values of another.
--
--   'balance' outputs a version of @input@, amplitude-modified so that its rms
--   power is equal to that of a comparator signal @comparator@. Thus a signal
--   that has suffered loss of power (eg., in passing through a filter
--   bank) can be restored by matching it with, for instance, its own
--   source. It should be noted that 'balance' provides amplitude modification
--   only - output signals are not altered in any other respect.
--
--   <https://csound.com/docs/manual/balance.html Csound documentation>
balance
  :: ARateSignal -- ^ @input@ - The input signal.
  -> ARateSignal -- ^ @comparator@ - The comparator signal.
  -> IRateSignal -- ^ @halfPowerPoint@ - The half-power point (in Hz) of a
                 --   special internal low-pass filter.
  -> IRateSignal -- ^ @disposition@ - The initial disposition of internal data
                 --   space. Since filtering incorporates a feedback loop of
                 --   previous output, the initial status of the storage space
                 --   used is significant. A zero value will clear the space;
                 --   a non-zero value will allow previous information to
                 --   remain.
  -> ARateSignal -- ^ The returned signal.
balance input comparator halfPowerPoint disposition = makeOpcodeSignal
  "balance"
  [ getSignal input
  , getSignal comparator
  , getSignal halfPowerPoint
  , getSignal disposition
  ]

-- | 'balanceWithDefaults' is identical to 'balance' with default values
--   supplied for @halfPowerPoint@ (@'i#' 10@) and @disposition@ (@'i#' 0@).
--
--   <https://csound.com/docs/manual/balance.html Csound documentation>
balanceWithDefaults
  :: ARateSignal -- ^ @input@ - The input signal.
  -> ARateSignal -- ^ @comparator@ - The comparator signal.
  -> ARateSignal -- ^ The returned signal.
balanceWithDefaults input comparator = makeOpcodeSignal
  "balance"
  [getSignal input, getSignal comparator, getSignal (i# 10), getSignal (i# 0)]

-- | Compress, limit, expand, duck or gate an audio signal.
--
--   This unit functions as an audio compressor, limiter, expander, or
--   noise gate, using either soft-knee or hard-knee mapping, and with
--   dynamically variable performance characteristics. It takes two audio
--   input signals, @input@ and @controller@, the first of which is modified by
--   a running analysis of the second. Both signals can be the same, or the
--   first can be modified by a different controlling signal.
--
--   'compress2' first examines the @controller@ by performing envelope
--   detection. This is directed by two control values @attack@ and @release@,
--   defining the attack and release time constants (in seconds) of the
--   detector. The detector rides the peaks (not the RMS) of the control
--   signal. Typical values are .01 and .1, the latter usually being similar to
--   @lookahead@.
--
--   The running envelope is next converted to decibels, then passed through a
--   mapping function to determine what compresser action (if any) should be
--   taken. The mapping function is defined by four decibel control values.
--   These are given as positive values, where 0 db corresponds to an amplitude
--   of @0dbfs@.
--
--   The @lowKnee@ and @highKnee@ breakpoints denote where compression or
--   expansion will begin. These set the boundaries of a soft-knee curve
--   joining the low-amplitude 1:1 line and the higher-amplitude
--   compression ratio line. Typical values are -52 and -30 db. If the two
--   breakpoints are equal, a hard-knee (angled) map will result.
--
--   The actions of 'compress2' will depend on the parameter settings given.
--   A hard-knee compressor-limiter, for instance, is obtained from
--   a near-zero attack time, equal-value break-points, and a very high
--   ratio (say 100). A noise-gate plus expander is obtained from some
--   positive threshold, and a fractional ratio above the knee.
--   A voice-activated music compressor (ducker) will result from feeding
--   the music into @input@ and the speech into @controller@. A voice
--   de-esser will result from feeding the voice into both, with the
--   @controller@ version being preceded by a band-pass filter that
--   emphasizes the sibilants. Each application will require some
--   experimentation to find the best parameter settings.
--
--   <https://csound.com/docs/manual/compress2.html Csound documentation>
compress2
  :: ARateSignal -- ^ @input@ - The input signal.
  -> ARateSignal -- ^ @controller@ - The controlling signal.
  -> KRateSignal -- ^ @threshold@ - The lowest decibel level that will be
                 --   allowed through. Normally -90 or less, but if higher
                 --   the threshold will begin removing low-level signal
                 --   energy such as background noise.
  -> KRateSignal -- ^ @lowKnee@ - The lower decibel breakpoint.
  -> KRateSignal -- ^ @highKnee@ - The higher decibel breakpoint.
  -> KRateSignal -- ^ @ratio@ - The ratio of compression when the signal level
                 --   is above the knee. The value 2 will advance the
                 --   output just one decibel for every input gain of two;
                 --   3 will advance just one in three; 20 just one in
                 --   twenty, etc. Inverse ratios will cause signal
                 --   expansion: .5 gives two for one, .25 four for one,
                 --   etc. The value 1 will result in no change.
  -> KRateSignal -- ^ @attack@ - The attack time (in seconds) of the envelope
                 --   detector.
  -> KRateSignal -- ^ @release@ - The release time (in seconds) of the envelope
                 --   detector.
  -> IRateSignal -- ^ @lookahead@ - The lookahead time in seconds, by which an
                 --   internal envelope release can sense what is coming.
                 --   This induces a delay between input and output, but
                 --   a small amount of lookahead improves the performance
                 --   of the envelope detector. Typical value is .05
                 --   seconds, sufficient to sense the peaks of the lowest
                 --   frequency in @controller@.
  -> ARateSignal -- ^ The returned signal.
compress2 input controller threshold lowKnee highKnee ratio attack release lookahead
  = makeOpcodeSignal
    "compress2"
    [ getSignal input
    , getSignal controller
    , getSignal threshold
    , getSignal lowKnee
    , getSignal highKnee
    , getSignal ratio
    , getSignal attack
    , getSignal release
    , getSignal lookahead
    ]
