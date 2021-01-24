module Schisma.Csound.Opcodes.MidiConverters
  ( ampmidi
  , ampmidiWithDefaults
  , cpsmidi
  , midiAmplitudeAndFrequency
  , rawMidiAmplitudeAndFrequency
  )
where

import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types

-- | Get the note number of the current MIDI event, expressed in
--   cycles-per-second.
--
--   <https://csound.com/docs/manual/cpsmidi.html Csound documentation>
cpsmidi :: IRateSignal -- ^ The returned signal.
cpsmidi = makeOpcodeSignal "cpsmidi" []

-- | Get the velocity of the current MIDI event, pass it through a normalized
--   translation table, and return an amplitude value in the range
--   0 - @scale@.
--
--   <https://csound.com/docs/manual/ampmidi.html Csound documentation>
ampmidi
  :: IRateSignal -- ^ @scale@ - The scaling factor.
  -> IRateSignal -- ^ @ftn@ - The function table number of a normalized
                 --   translation table, by which the incoming value is
                 --   first interpreted. A value of zero denotes no
                 --   translation.
  -> IRateSignal -- ^ The returned signal.
ampmidi scale ftn = makeOpcodeSignal "ampmidi" $ map getSignal [scale, ftn]

-- | 'ampmidiWithDefaults' is identical to 'ampmidi' with a default value
--   supplied for @ftn@ (@'i#' 0@, which denotes no translation).
--
--   <https://csound.com/docs/manual/ampmidi.html Csound documentation>
ampmidiWithDefaults
  :: IRateSignal    -- ^ @scale@ - The scaling factor.
  -> IRateSignal -- ^ The returned signal.
ampmidiWithDefaults scale =
  makeOpcodeSignal "ampmidi" $ map getSignal [scale, i# 0]

-- | Determines the amplitude and frequency of an incoming MIDI note sent
--   on the same channel as @channel@.
--
--   Two signals are returned. The first signal contains the amplitude
--   (scaled from @0 - 1@), and the second signal contains the frequency
--   (assuming 12-TET, A440, and that Middle C is MIDI note number 60).
midiAmplitudeAndFrequency
  :: KRateSignal   -- ^ @channel@ - The channel to check.
  -> [KRateSignal] -- ^ The returned signals.
midiAmplitudeAndFrequency channel = map toSignal [1, 2] where
  args     = [getSignal channel]
  opcode   = IncludedOpcode "midiAmplitudeAndFrequency" args [KRate, KRate]
  toSignal = KRateSignal . Signal opcode

-- | Returns the raw amplitude and frequency values of an incoming MIDI
--   note sent on the same channel as @channel@.
--
--   Two signals are returned. The first signal contains the amplitude
--   (from @0 - 127@), and the second signal contains the frequency
--   (also from @0 - 127@).
rawMidiAmplitudeAndFrequency
  :: KRateSignal   -- ^ @channel@ - The channel to check.
  -> [KRateSignal] -- ^ The returned signals.
rawMidiAmplitudeAndFrequency channel = map toSignal [1, 2] where
  args     = [getSignal channel]
  opcode   = IncludedOpcode "rawMidiAmplitudeAndFrequency" args [KRate, KRate]
  toSignal = KRateSignal . Signal opcode

