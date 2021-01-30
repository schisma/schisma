module Schisma.Csound.Opcodes.Oscillators
  ( LowFrequencyOscillator(..)
  , Oscillator(..)
  , PrecisionOscillator(..)
  , SyncOscillator(..)
  , osciln
  , osciliktp
  , osciliktpWithDefaults
  , vco2
  , vco2WithDefaults
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , i#
                                                , k#
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , IRateSignal
                                                , IsSignal(..)
                                                , KRateSignal
                                                )

class (SignalGenerator a) => LowFrequencyOscillator a where
  -- | A low frequency oscillator of various shapes.
  --
  --   <https://csound.com/docs/manual/lfo.html Csound documentation>
  lfo
    :: KRateSignal -- ^ @amplitude@ - The amplitude.
    -> KRateSignal -- ^ @frequency@ - The frequency in cycles per second.
    -> IRateSignal -- ^ @waveform@ - The waveform of the oscillator. The types
                   --   are as follows:
                   --
                   --   * 0: sine
                   --   * 1: triangles
                   --   * 2: square (bipolar)
                   --   * 3: square (unipolar)
                   --   * 4: saw-tooth
                   --   * 5: saw-tooth (down)
                   --
                   --   The sine wave is implemented as a 4096 table and
                   --   linear interpolation. The others are calculated.
    -> a           -- ^ The returned signal.
  lfo amplitude frequency waveform = makeOpcodeSignal "lfo" args where
    args =
      [ getSignal amplitude
      , getSignal frequency
      , getSignal waveform
      ]

  -- | 'lfoWithDefaults' is identical to 'lfo' with a default value supplied
  --   for @waveform@ (@'i#' 0@, which indicates a sinewave).
  --
  --   <https://csound.com/docs/manual/lfo.html Csound documentation>
  lfoWithDefaults
    :: KRateSignal -- ^ @amplitude@ - The amplitude.
    -> KRateSignal -- ^ @frequency@ - The frequency in cycles per second.
    -> a           -- ^ The returned signal.
  lfoWithDefaults amplitude frequency = lfo amplitude frequency (i# 0)

instance LowFrequencyOscillator ARateSignal
instance LowFrequencyOscillator KRateSignal


class (IsSignal a, IsSignal b, SignalGenerator c) => Oscillator a b c where
  -- | A simple oscillator.
  --
  --   'oscil' reads table @ftn@ sequentially and repeatedly at a frequency
  --   @frequency@.  The amplitude is scaled by @amplitude@.
  --
  --   The 'oscil' opcode generates periodic control (or audio) signals
  --   consisting of the value of @amplitude@ times the value returned from
  --   control rate (audio rate) sampling of a stored function table. The
  --   internal phase is simultaneously advanced in accordance with the
  --   @frequency@ input value.
  --
  --   Table @ftn@ is incrementally sampled modulo the table length and the
  --   value obtained is multiplied by @amplitude@.
  --
  --   If you need to change the oscillator table with a k-rate signal, you can
  --   use 'oscilikt'.
  --
  --   <https://csound.com/docs/manual/oscil.html Csound documentation>
  oscil
    :: a           -- ^ @amplitude@ - The amplitude.
    -> b           -- ^ @frequency@ - The frequency in cycles per second.
    -> IRateSignal -- ^ @ftn@ - The function table number. The function table
                   --   requires a wrap-around guard point (i.e., its size
                   --   should be an exact power of 2).
    -> IRateSignal -- ^ @phase@ - The initial phase of sampling, expressed as a
                   --   fraction of a cycle (0 to 1). A negative value will
                   --   cause phase initialization to be skipped.
    -> c           -- ^ The returned signal.
  oscil amplitude frequency ftn phase = makeOpcodeSignal "oscil" args where
    args =
      [ getSignal amplitude
      , getSignal frequency
      , getSignal ftn
      , getSignal phase
      ]

  -- | 'oscilWithDefaults' is identical to 'oscil' with default values supplied
  --   for @ftn@ (@'i#' (-1)@, which indicates a sinewave) and @phase@
  --   (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/oscil.html Csound documentation>
  oscilWithDefaults
    :: a -- ^ @amplitude@ - The amplitude.
    -> b -- ^ @frequency@ - The frequency in cycles per second.
    -> c -- ^ The returned signal.
  oscilWithDefaults amplitude frequency =
    oscil amplitude frequency (i# (-1)) (i# 0)

  -- | A simple oscillator with linear interpolation.
  --
  --   'oscili' reads table @ftn@ sequentially and repeatedly at a frequency
  --   @frequency@.  The amplitude is scaled by @amplitude@. Linear
  --   interpolation is applied for table look up from internal phase values.
  --
  --   'oscili' differs from 'oscil' in that the standard procedure of using a
  --   truncated phase as a sampling index is here replaced by a process that
  --   interpolates between two successive lookups. Interpolating generators
  --   will produce a noticeably cleaner output signal, but they may take as
  --   much as twice as long to run. Adequate accuracy can also be gained
  --   without the time cost of interpolation by using large stored function
  --   tables of 2K, 4K or 8K points if the space is available.
  --
  --   Table @ftn@ is incrementally sampled modulo the table length and the
  --   value obtained is multiplied by @amplitude@.
  --
  --   If you need to change the oscillator table with a k-rate signal, you can
  --   use 'oscilikt'.
  --
  --   <https://csound.com/docs/manual/oscili.html Csound documentation>
  oscili
    :: a           -- ^ @amplitude@ - The amplitude.
    -> b           -- ^ @frequency@ - The frequency in cycles per second.
    -> IRateSignal -- ^ @ftn@ - The function table number. The function table
                   --   requires a wrap-around guard point (i.e., its size
                   --   should be an exact power of 2).
    -> IRateSignal -- ^ @phase@ - The initial phase of sampling, expressed as a
                   --   fraction of a cycle (0 to 1). A negative value will
                   --   cause phase initialization to be skipped.
    -> c           -- ^ The returned signal.
  oscili amplitude frequency ftn phase = makeOpcodeSignal "oscili" args where
    args =
      [ getSignal amplitude
      , getSignal frequency
      , getSignal ftn
      , getSignal phase
      ]

  -- | 'osciliWithDefaults' is identical to 'oscili' with default values
  --   supplied for @ftn@ (@'i#' (-1)@, which indicates a sinewave) and @phase@
  --   (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/oscili.html Csound documentation>
  osciliWithDefaults
    :: a -- ^ @amplitude@ - The amplitude.
    -> b -- ^ @frequency@ - The frequency in cycles per second.
    -> c -- ^ The returned signal.
  osciliWithDefaults amplitude frequency =
    oscili amplitude frequency (i# (-1)) (i# 0)

  -- | A linearly interpolated oscillator that allows changing the table
  --   number at k-rate.
  --
  --   'oscilikt' is very similar to 'oscili', but allows changing the table
  --   number at k-rate. It is slightly slower than 'oscili' (especially with
  --   high control rate), although also more accurate as it uses a 31-bit
  --   phase accumulator, as opposed to the 24-bit one used by 'oscili'.
  --
  --   <https://csound.com/docs/manual/oscilikt.html Csound documentation>
  oscilikt
    :: a           -- ^ @amplitude@ - The amplitude.
    -> b           -- ^ @frequency@ - The frequency in cycles per second.
    -> KRateSignal -- ^ @ftn@ - The function table number. Can be varied at
                   --   control rate.
    -> IRateSignal -- ^ @phase@ - The initial phase in the range of 0 to 1.
                   --   Other values are wrapped to the allowed range.
    -> IRateSignal -- ^ @skipInit@ - Should initialization be skipped?
    -> c           -- ^ The returned signal.
  oscilikt amplitude frequency ftn phase skipInit = makeOpcodeSignal "oscilikt" args where
    args =
      [ getSignal amplitude
      , getSignal frequency
      , getSignal ftn
      , getSignal phase
      , getSignal skipInit
      ]

  -- | 'osciliktWithDefaults' is identical to 'oscilikt' with default values
  --   supplied for @phase@ (@'i#' 0@) and @skipInit@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/oscilikt.html Csound documentation>
  osciliktWithDefaults
    :: a           -- ^ @amplitude@ - The amplitude.
    -> b           -- ^ @frequency@ - The frequency in cycles per second.
    -> KRateSignal -- ^ @ftn@ - The function table number. Can be varied at
                   --   control rate.
    -> c           -- ^ The returned signal.
  osciliktWithDefaults amplitude frequency ftn =
    oscilikt amplitude frequency ftn (i# 0) (i# 0)

  -- | A simple oscillator with cubic interpolation.
  --
  --   'oscil3' reads table @ftn@ sequentially and repeatedly at a frequency
  --   @frequency@.  The amplitude is scaled by @amplitude@. Cubic
  --   interpolation is applied for table look up from internal phase
  --   values.
  --
  --   'oscil3' is identical to 'oscili', except that it uses cubic
  --   interpolation.
  --
  --   Table @ftn@ is incrementally sampled modulo the table length and the
  --   value obtained is multiplied by @amplitude@.
  --
  --   If you need to change the oscillator table with a k-rate signal, you can
  --   use 'oscilikt'.
  --
  --   <https://csound.com/docs/manual/oscil3.html Csound documentation>
  oscil3
    :: a           -- ^ @amplitude@ - The amplitude.
    -> b           -- ^ @frequency@ - The frequency in cycles per second.
    -> IRateSignal -- ^ @ftn@ - The function table number. The function table
                   --   requires a wrap-around guard point (i.e., its size
                   --   should be an exact power of 2).
    -> IRateSignal -- ^ @phase@ - The initial phase of sampling, expressed as a
                   --   fraction of a cycle (0 to 1). A negative value will
                   --   cause phase initialization to be skipped.
    -> c           -- ^ The returned signal.
  oscil3 amplitude frequency ftn phase = makeOpcodeSignal "oscil3" args where
    args =
      [ getSignal amplitude
      , getSignal frequency
      , getSignal ftn
      , getSignal phase
      ]

  -- | 'oscil3WithDefaults' is identical to 'oscil3' with default values
  --   supplied for @ftn@ (@'i#' (-1)@, which indicates a sinewave) and @phase@
  --   (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/oscil3.html Csound documentation>
  oscil3WithDefaults
    :: a -- ^ @amplitude@ - The amplitude.
    -> b -- ^ @frequency@ - The frequency in cycles per second.
    -> c -- ^ The returned signal.
  oscil3WithDefaults amplitude frequency =
    oscil3 amplitude frequency (i# (-1)) (i# 0)

instance Oscillator ARateSignal ARateSignal ARateSignal
instance Oscillator KRateSignal ARateSignal ARateSignal
instance Oscillator IRateSignal ARateSignal ARateSignal

instance Oscillator ARateSignal KRateSignal ARateSignal
instance Oscillator KRateSignal KRateSignal ARateSignal
instance Oscillator IRateSignal KRateSignal ARateSignal

instance Oscillator ARateSignal IRateSignal ARateSignal
instance Oscillator KRateSignal IRateSignal ARateSignal
instance Oscillator IRateSignal IRateSignal ARateSignal

instance Oscillator KRateSignal KRateSignal KRateSignal


class (IsSignal a, IsSignal b, SignalGenerator c) => PrecisionOscillator a b c where
  -- | High precision oscillator.
  --
  --   'poscil' (precise oscillator) is the same as 'oscili', but allows much
  --   more precise frequency control, especially when using long tables
  --   and low frequency values. It uses floating-point table indexing,
  --   instead of integer math, like 'oscil' and 'oscili'. It is only a bit
  --   slower than 'oscili'.
  --
  --   'poscil' can accept also negative frequency values and use a-rate
  --   values both for amplitude and frequency. So both AM and FM are
  --   allowed using this opcode.
  --
  --   Note that 'poscil' can use deferred (non-power of two) length tables.
  --
  --   <https://csound.com/docs/manual/poscil.html Csound documentation>
  poscil
    :: a           -- ^ @amplitude@ - The amplitude.
    -> b           -- ^ @frequency@ - The frequency in cycles per second.
    -> IRateSignal -- ^ @ftn@ - The function table number.
    -> IRateSignal -- ^ @phase@ - The initial phase of sampling, expressed as a
                   --   fraction of a cycle (0 to 1). A negative value will
                   --   cause phase initialization to be skipped.
    -> c           -- ^ The returned signal.
  poscil amplitude frequency ftn phase = makeOpcodeSignal "poscil" args where
    args =
      [ getSignal amplitude
      , getSignal frequency
      , getSignal ftn
      , getSignal phase
      ]

  -- | 'poscilWithDefaults' is identical to 'poscil' with default values
  --   supplied for @ftn@ (@'i#' (-1)@, which indicates a sinewave) and @phase@
  --   (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/poscil.html Csound documentation>
  poscilWithDefaults
    :: a -- ^ @amplitude@ - The amplitude.
    -> b -- ^ @frequency@ - The frequency in cycles per second.
    -> c -- ^ The returned signal.
  poscilWithDefaults amplitude frequency =
    poscil amplitude frequency (i# (-1)) (i# 0)

  -- | High precision oscillator with cubic interpolation.
  --
  --   'poscil3' works like 'poscil', but uses cubic interpolation.
  --
  --   Note that 'poscil3' can use deferred (non-power of two) length tables.
  --
  --   <https://csound.com/docs/manual/poscil3.html Csound documentation>
  poscil3
    :: a           -- ^ @amplitude@ - The amplitude.
    -> b           -- ^ @frequency@ - The frequency in cycles per second.
    -> IRateSignal -- ^ @ftn@ - The function table number.
    -> IRateSignal -- ^ @phase@ - The initial phase of sampling, expressed as a
                   --   fraction of a cycle (0 to 1). A negative value will
                   --   cause phase initialization to be skipped.
    -> c           -- ^ The returned signal.
  poscil3 amplitude frequency ftn phase = makeOpcodeSignal "poscil3" args where
    args =
      [ getSignal amplitude
      , getSignal frequency
      , getSignal ftn
      , getSignal phase
      ]

  -- | 'poscil3WithDefaults' is identical to 'poscil3' with default values
  --   supplied for @ftn@ (@'i#' (-1)@, which indicates a sinewave) and @phase@
  --   (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/poscil.html Csound documentation>
  poscil3WithDefaults
    :: a -- ^ @amplitude@ - The amplitude.
    -> b -- ^ @frequency@ - The frequency in cycles per second.
    -> c -- ^ The returned signal.
  poscil3WithDefaults amplitude frequency =
    poscil3 amplitude frequency (i# (-1)) (i# 0)

instance PrecisionOscillator ARateSignal ARateSignal ARateSignal
instance PrecisionOscillator ARateSignal KRateSignal ARateSignal
instance PrecisionOscillator KRateSignal ARateSignal ARateSignal
instance PrecisionOscillator KRateSignal KRateSignal ARateSignal
instance PrecisionOscillator KRateSignal KRateSignal KRateSignal
instance PrecisionOscillator KRateSignal KRateSignal IRateSignal


class (IsSignal a, IsSignal b) => SyncOscillator a b where
  -- | A linearly interpolated oscillator with sync status that allows
  --   changing the table number at k-rate.
  --
  --   'oscilikts' is the same as 'oscilikt'. Except it has a sync input
  --   that can be used to re-initialize the oscillator to a k-rate phase
  --   value. It is slower than 'oscilikt' and 'osciliktp'.
  --
  --   <https://csound.com/docs/manual/oscilikts.html Csound documentation>
  oscilikts
    :: a           -- ^ @amplitude@ - The amplitude.
    -> b           -- ^ @frequency@ - The frequency in cycles per second.
                   --   Zero and negative values are allowed. However, the
                   --   absolute value must be less than @sr@ (and
                   --   recommended to be less than @sr@/2).
    -> KRateSignal -- ^ @ftn@ - The function table number. Can be varied at
                   --   control rate (useful to "morph" waveforms, or select from
                   --   a set of band-limited tables generated by GEN30).
    -> ARateSignal -- ^ @sync@ - any positive value resets the phase of
                   --   'oscilikts' to @phase@. Zero or negative values
                   --   have no effect.
    -> KRateSignal -- ^ @phase@ - Sets the phase, initially and when it is
                   --   re-initialized with @sync@.
    -> IRateSignal -- ^ @skipInit@ - Should initialization be skipped?
    -> ARateSignal -- ^ The returned signal.
  oscilikts amplitude frequency ftn sync phase skipInit = makeOpcodeSignal "oscilikts" args where
    args =
      [ getSignal amplitude
      , getSignal frequency
      , getSignal ftn
      , getSignal sync
      , getSignal phase
      , getSignal skipInit
      ]

  -- | 'osciliktsWithDefaults' is identical to 'oscilikts' with a default value
  --   supplied for @skipInit@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/oscilikts.html Csound documentation>
  osciliktsWithDefaults
    :: a           -- ^ @amplitude@ - The amplitude.
    -> b           -- ^ @frequency@ - The frequency in cycles per second.
                   --   Zero and negative values are allowed. However, the
                   --   absolute value must be less than @sr@ (and
                   --   recommended to be less than @sr@/2).
    -> KRateSignal -- ^ @ftn@ - The function table number. Can be varied at
                   --   control rate (useful to "morph" waveforms, or select from
                   --   a set of band-limited tables generated by GEN30).
    -> ARateSignal -- ^ @sync@ - any positive value resets the phase of
                   --   'oscilikts' to @phase@. Zero or negative values
                   --   have no effect.
    -> KRateSignal -- ^ @phase@ - Sets the phase, initially and when it is
                   --   re-initialized with @sync@.
    -> ARateSignal -- ^ The returned signal.
  osciliktsWithDefaults amplitude frequency ftn sync phase =
    oscilikts amplitude frequency ftn sync phase (i# 0)

instance SyncOscillator ARateSignal ARateSignal
instance SyncOscillator KRateSignal ARateSignal
instance SyncOscillator IRateSignal ARateSignal

instance SyncOscillator ARateSignal KRateSignal
instance SyncOscillator KRateSignal KRateSignal
instance SyncOscillator IRateSignal KRateSignal

instance SyncOscillator ARateSignal IRateSignal
instance SyncOscillator KRateSignal IRateSignal
instance SyncOscillator IRateSignal IRateSignal


-- | Accesses table values at a user-defined frequency. 'osciln' will sample
--   several times through the stored table @ftn@ at a rate of @frequency@
--   times per second, after which it will output zeros. Generates audio
--   signals only, with output values scaled by @amplitude@.
--
--   <https://csound.com/docs/manual/osciln.html Csound documentation>
osciln
  :: KRateSignal -- ^ @amplitude@ - The amplitude.
  -> IRateSignal -- ^ @frequency@ - The rate through the table @ftn@.
  -> IRateSignal -- ^ @ftn@ - The function table number.
  -> IRateSignal -- ^ @count@ - The number of times through the table @ftn@.
  -> ARateSignal -- ^ The returned signal.
osciln amplitude frequency ftn count = makeOpcodeSignal "osciln" args where
  args =
    [getSignal amplitude, getSignal frequency, getSignal ftn, getSignal count]

-- | A linearly interpolated oscillator that allows allows phase modulation.
--
--   'osciliktp' allows phase modulation (which is actually implemented as
--   k-rate frequency modulation, by differentiating phase input). The
--   disadvantage is that there is no amplitude control, and frequency can
--   be varied only at the control-rate. This opcode can be faster or
--   slower than 'oscilikt', depending on the control-rate.
--
--   <https://csound.com/docs/manual/osciliktp.html Csound documentation>
osciliktp
  :: KRateSignal -- ^ @frequency@ - The frequency in cycles per second.
  -> KRateSignal -- ^ @ftn@ - The function table number. Can be varied at
                 --   control rate (useful to "morph" waveforms, or select from
                 --   a set of band-limited tables generated by GEN30).
  -> KRateSignal -- ^ @phase@ - The phase. The expected range is 0 to 1. The
                 --   absolute value of the difference of the current and
                 --   previous value of @phase@ must be less than @ksmps@.
  -> IRateSignal -- ^ @skipInit@ - Should initialization be skipped?
  -> ARateSignal -- ^ The returned signal.
osciliktp frequency ftn phase skipInit = makeOpcodeSignal "osciliktp" args where
  args =
    [getSignal frequency, getSignal ftn, getSignal phase, getSignal skipInit]

-- | 'osciliktpWithDefaults' is identical to 'osciliktp' with a default value
--   supplied for @skipInit@ (@'i#' 0@).
--
--   <https://csound.com/docs/manual/osciliktp.html Csound documentation>
osciliktpWithDefaults
  :: KRateSignal -- ^ @frequency@ - The frequency in cycles per second.
  -> KRateSignal -- ^ @ftn@ - The function table number. Can be varied at
                 --   control rate (useful to "morph" waveforms, or select from
                 --   a set of band-limited tables generated by GEN30).
  -> KRateSignal -- ^ @phase@ - The phase. The expected range is 0 to 1. The
                 --   absolute value of the difference of the current and
                 --   previous value of @phase@ must be less than @ksmps@.
  -> ARateSignal -- ^ The returned signal.
osciliktpWithDefaults frequency ftn phase =
  osciliktp frequency ftn phase (i# 0)

-- | 'vco2' is similar to 'vco'. But the implementation uses pre-calculated
--   tables of band-limited waveforms (see also GEN30) rather than
--   integrating impulses. This opcode can be faster than 'vco' (especially if
--   a low control-rate is used) and also allows better sound quality.
--   Additionally, there are more waveforms and oscillator phase can be
--   modulated at k-rate. The disadvantage is increased memory usage. For
--   more details about vco2 tables, see also 'vco2init' and 'vco2ft'.
--
--   When a low control-rate is used, pulse width (@pulseWidth@) and phase
--   (@phase@) modulation is internally converted to frequency modulation. This
--   allows for faster processing and reduced artifacts. But in the case of
--   very long notes and continuous fast changes in @pulseWidth@ or @phase@,
--   the phase may drift away from the requested value. In most cases, the
--   phase error is at most 0.037 per hour (assuming a sample rate of 44100
--   Hz).
--
--   This is a problem mainly in the case of @pulseWidth@, where it
--   may result in various artifacts. While future releases of 'vco2' may fix
--   such errors, the following work-arounds may also be of some help:
--
--   * Use @pulseWidth@ values only in the range 0.05 to 0.95. (There are more
--     artifacts around integer values.)
--
--   * Try to avoid modulating @pulseWidth@ by asymmetrical waveforms like
--     a sawtooth wave. Relatively slow (<= 20 Hz) symmetrical modulation
--     (e.g. sine or triangle), random splines (also slow), or a fixed pulse
--     width is a lot less likely to cause synchronization problems.

--   * In some cases, adding random jitter (for example: random splines
--     with an amplitude of about 0.01) to @pulseWidth@ may also fix the
--     problem.
--
--   <https://csound.com/docs/manual/vco2.html Csound documentation>
vco2
  :: KRateSignal -- ^ @amplitude@ - The amplitude amplitude scale.
                 --   In the case of a @mode@ value of 6 (a pulse
                 --   waveform), the actual output level can be a lot higher
                 --   than this value.
  -> KRateSignal -- ^ @frequency@ - The frequency.
  -> IRateSignal -- ^ @mode@ - The mode, which is a sum of values
                 --   representing the waveform and its control values.
                 --
                 --   One may use any of the following values for @mode@:
                 --
                 --   * 16: enable k-rate phase control (if set, kphs is
                 --   a required k-rate parameter that allows phase
                 --   modulation)
                 --   * 1: skip initialization
                 --
                 --   One may use exactly one of these @mode@ values to
                 --   select the waveform to be generated:
                 --
                 --   * 14: user defined waveform -1 (requires using the
                 --     'vco2init' opcode)
                 --   * 12: triangle (no ramp, faster)
                 --   * 10: square wave (no PWM, faster)
                 --   * 8: 4 * x * (1 - x) (i.e. integrated sawtooth)
                 --   * 6: pulse (not normalized)
                 --   * 4: sawtooth \/ triangle \/ ramp
                 --   * 2: square / PWM
                 --   * 0: sawtooth
  -> KRateSignal -- ^ @pulseWidth@ - The pulse width of the square wave
                 --   (@mode = 2@) or the ramp characteristics of the
                 --   triangle wave (@mode = 4@). It is required only
                 --   by these waveforms and ignored in all other cases.
                 --   The expected range is 0 to 1, any other value is
                 --   wrapped to the allowed range. Note that @pulseWidth@
                 --   must not be an exact integer value (e.g. 0 or 1) if a
                 --   sawtooth \/ triangle \/ ramp (@mode = 4@) is generated.
                 --   In this case, the recommended range is about 0.01 to
                 --   0.99. There is no such limitation for a square/PWM
                 --   waveform.
  -> KRateSignal -- ^ @phase@ - The oscillator phase. Similarly to
                 --   @pulseWidth@, the expected range is 0 to 1.
  -> IRateSignal -- ^ @bandwidth@ - The bandwidth of the generated waveform,
                 --   as a percentage (0 to 1) of the sample rate. The expected
                 --   range is 0 to 0.5 (i.e. up to @sr/2@). Other values are
                 --   limited to the allowed range. Setting @bandwidth@ to 0.25
                 --   (@sr/4@), or 0.3333 (@sr/3@) can produce a "fatter"
                 --   sound in some cases, although it is more likely to reduce
                 --   quality.
  -> ARateSignal -- ^ The returned signal.
vco2 amplitude frequency mode pulseWidth phase bandwidth = makeOpcodeSignal
  "vco2"
  args where
  args =
    [ getSignal amplitude
    , getSignal frequency
    , getSignal mode
    , getSignal pulseWidth
    , getSignal phase
    , getSignal bandwidth
    ]

-- | 'vco2WithDefaults' is identical to 'vco2' with default values
--   supplied for @mode@ (@'i#' 0@, which indicates a sawtooth wave with
--   no k-rate phase control), @pulseWidth@ (@'k#' 0@), @phase@ (@'k#'
--   0@), and @bandWidth@ (@'i#' 0.5@).
--
--   <https://csound.com/docs/manual/vco2.html Csound documentation>
vco2WithDefaults
  :: KRateSignal -- ^ @amplitude@ - The amplitude amplitude scale.
                 --   In the case of a @mode@ value of 6 (a pulse
                 --   waveform), the actual output level can be a lot higher
                 --   than this value.
  -> KRateSignal -- ^ @frequency@ - The frequency.
  -> ARateSignal -- ^ The returned signal.
vco2WithDefaults amplitude frequency =
  vco2 amplitude frequency (i# 0) (k# 0) (k# 0) (i# 0.5)
