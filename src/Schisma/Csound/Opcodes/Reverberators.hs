module Schisma.Csound.Opcodes.Reverberators
  ( babo
  , baboWithDefaults
  , freeverb
  , freeverbWithDefaults
  ) where

import           Schisma.Csound.Opcodes.FunctionTables
                                                ( ftgenonce )
import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , makeOpcodeSignals
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , IRateSignal
                                                , IsSignal(getSignal)
                                                , KRateSignal
                                                )

-- | A physical model reverberator.
--
--   'babo' stands for /ba/ll-within-the-/bo/x. It is a physical model
--   reverberator based on the <https://sci-hub.tw/10.2307/3680990 paper> by
--   Davide Rocchesso "The Ball within the Box: a sound-processing metaphor",
--   Computer Music Journal, Vol 19, N.4, pp.45-47, Winter 1995.
--
--   The resonator geometry can be defined along with some response
--   characteristics, the position of the listener within the resonator, and
--   the position of the sound source.
--
--   <https://csound.com/docs/manual/babo.html Csound documentation>
babo
  :: ARateSignal    -- ^ @inputSignal@ - The input signal.
  -> (KRateSignal, KRateSignal, KRateSignal)    -- ^ @(x, y, z)@ - The virtual coordinates of the source of
                    --   sound (i.e., the @inputSignal@ signal).
  -> (IRateSignal, IRateSignal, IRateSignal)   -- ^ @(width, depth, height)@ - The coordinates of the
                    --   geometry of the resonator (length of the edges in
                    --   meters).
  -> IRateSignal    -- ^ @diffusion@ - The coefficient of diffusion at the
                    --   walls, which regulates the amount of diffusion (0-1,
                    --   where 0 = no diffusion and 1 = maximum diffusion).
  -> IRateSignal    -- ^ @ftn@ - The function table number that holds all the
                    --   additional parameters of the resonator. This is
                    --   typically a GEN2-type function used in non-rescaling
                    --   mode. The parameters are as follows:
                    --
                    --   * decay - main decay of the resonator
                    --
                    --   * hydecay - high frequency decay of the resonator
                    --
                    --   * rcvx, rcvy, rcvz - the coordinates of the position
                    --     of the receiver (the listener) (in meters; 0, 0, 0
                    --     is the resonator center)
                    --
                    --   * rdistance - the distance in meters between the two
                    --     pickups (your ears, for example)
                    --
                    --   * direct - the attenuation of the direct signal (0-1)
                    --
                    --   * early_diff - the attenuation coefficient of the
                    --     early reflections (0-1)
  -> [ARateSignal]  -- ^ The returned signals.
babo inputSignal (x, y, z) (width, depth, height) diffusion ftn =
  makeOpcodeSignals "babo" args 2 where
  args = [getSignal inputSignal] ++ map getSignal [x, y, z] ++ map
    getSignal
    [width, depth, height, diffusion, ftn]

-- | 'baboWithDefaults' is identical to 'babo' with default values supplied
--   for @diffusion@ (@'i#' 1@) and @ftn@, which is a GEN2-type (technically
--   @'i#' (-2)@ to omit rescaling) function whose default values are as
--   follows:
--
-- * decay: 0.99
-- * hydecay: 0.1
-- * rcvx, rcvy, rcvz: 0, 0, 0
-- * rdistance: 0.3
-- * direct: 0.5
-- * early_diff: 0.8
--
--   <https://csound.com/docs/manual/babo.html Csound documentation>
baboWithDefaults
  :: ARateSignal    -- ^ @inputSignal@ - The input signal.
  -> (KRateSignal, KRateSignal, KRateSignal)    -- ^ @(x, y, z)@ - The virtual coordinates of the source of
                    --   sound (i.e., the @inputSignal@ signal).
  -> (IRateSignal, IRateSignal, IRateSignal)   -- ^ @(width, depth, height)@ - The coordinates of the
                    --   geometry of the resonator (length of the edges in
                    --   meters).
  -> [ARateSignal]  -- ^ The returned signals.
baboWithDefaults inputSignal (x, y, z) (width, depth, height) = babo
  inputSignal
  (x    , y    , z)
  (width, depth, height)
  (i# 1.0)
  ftn
 where
  ftn = ftgenonce (i# 8) (i# (-2)) (map i# [0.99, 0.1, 0, 0, 0, 0.3, 0.5, 0.8])


-- | Opcode version of Jezar's Freeverb.
--
--   'freeverb' is a stereo reverb unit based on Jezar's public domain C++
--   sources, composed of eight parallel comb filters on both channels,
--   followed by four allpass units in series. The filters on the right
--   channel are slightly detuned compared to the left channel in order to
--   create a stereo effect.
--
--   <https://csound.com/docs/manual/freeverb.html Csound documentation>
freeverb
  :: ARateSignal    -- ^ @inputLeft@ - The left input signal.
  -> ARateSignal    -- ^ @inputRight@ - The right input signal. Usually this
                    --   is the same as @inputLeft@, but different inputs
                    --   can be used for special effect.
  -> KRateSignal    -- ^ @roomSize@ - The length of the reverb. A higher value
                    --   means longer reverb. Range is from 0 to 1.
                    --   Settings above 1 may make the opcode unstable.
  -> KRateSignal    -- ^ @highFrequencyDamp@ - High frequency attenuation. A
                    --   value of zero means all frequencies decay at the same
                    --   rate, while higher settings will result in
                    --   a faster decay of the high frequency range. Range
                    --   is from 0 to 1.
  -> IRateSignal    -- ^ @sampleRate@ - Adjusts the reverb parameters for use
                    --   with the specified sample rate (this will affect
                    --   the length of the delay lines in samples, and the
                    --   high frequency attenuation). Only integer
                    --   multiples of 44100 will reproduce the original
                    --   character of the reverb exactly, so it may be
                    --   useful to set this to 44100 or 88200 for an
                    --   orchestra sample rate of 48000 or 96000 Hz,
                    --   respectively. While @sampleRate@ is normally
                    --   expected to be close to the orchestra sample rate,
                    --   different settings may be useful for special effects.
  -> IRateSignal    -- ^ @skipInit@ - Should initialization be skipped?
  -> [ARateSignal]  -- ^ The returned signals.
freeverb inputLeft inputRight roomSize highFrequencyDamp sampleRate skipInit =
  makeOpcodeSignals "freeverb" args 2 where
  args =
    [ getSignal inputLeft
    , getSignal inputRight
    , getSignal roomSize
    , getSignal highFrequencyDamp
    , getSignal sampleRate
    , getSignal skipInit
    ]

-- | 'freeverbWithDefaults' is identical to 'freeverb' with default values
--   supplied for @sampleRate@ (@'i#' 44100@) and @skipInit@ (@'i#' 0@).
--
--   <https://csound.com/docs/manual/freeverb.html Csound documentation>
freeverbWithDefaults
  :: ARateSignal    -- ^ @inputLeft@ - The left input signal.
  -> ARateSignal    -- ^ @inputRight@ - The right input signal. Usually this
                    --   is the same as @inputLeft@, but different inputs
                    --   can be used for special effect.
  -> KRateSignal    -- ^ @roomSize@ - The length of the reverb. A higher value
                    --   means longer reverb. Range is from 0 to 1.
                    --   Settings above 1 may make the opcode unstable.
  -> KRateSignal    -- ^ @highFrequencyDamp@ - High frequency attenuation. A
                    --   value of zero means all frequencies decay at the same
                    --   rate, while higher settings will result in
                    --   a faster decay of the high frequency range. Range
                    --   is from 0 to 1.
  -> [ARateSignal]  -- ^ The returned signals.
freeverbWithDefaults inputLeft inputRight roomSize highFrequencyDamp =
  freeverb inputLeft inputRight roomSize highFrequencyDamp (i# 44100) (i# 0)
