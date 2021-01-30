module Schisma.Csound.Opcodes.PitchConverters
  ( Cps(..)
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , i#
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types.Signals   ( IRateSignal
                                                , IsSignal(..)
                                                , KRateSignal
                                                )

class (IsSignal a, SignalGenerator a) => Cps a where
  -- | Converts a MIDI note number value to cycles-per-second.
  --
  --   'cpsmidinn' is a function that takes an i-rate or k-rate value
  --   representing a MIDI note number and returns the equivalent frequency
  --   value in cycles-per-second (Hertz). This conversion assumes that
  --   Middle C is MIDI note number 60 and that Middle A is tuned to 440
  --   Hz. MIDI note number values are typically integers in the range from
  --   0 to 127 but fractional values or values outside of this range will
  --   be interpreted consistently.
  --
  --   <https://csound.com/docs/manual/cpsmidinn.html Csound documentation>
  cpsmidinn
    :: a -- ^ @midiNoteNumber@ - The MIDI note number (0-127).
    -> a -- ^ The returned signal.
  cpsmidinn midiNoteNumber =
    makeOpcodeSignal "cpsmidinn" [getSignal midiNoteNumber]

  -- | Converts a frequency to MIDI note number, taking the global value of A4
  --   into account.
  --
  --   <https://csound.com/docs/manual/ftom.html Csound documentation>
  ftom
    :: a           -- ^ @frequency@ - The frequency.
    -> IRateSignal -- ^ @round@ - Rounds to the nearest integer if non-zero.
    -> a           -- ^ The returned signal.
  ftom frequency round =
    makeOpcodeSignal "ftom" [getSignal frequency, getSignal round]

  -- | 'ftomWithDefaults' is identical to 'ftom' with a default value
  --   supplied for @round@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/ftom.html Csound documentation>
  ftomWithDefaults
    :: a -- ^ @frequency@ - The frequency.
    -> a -- ^ The returned signal.
  ftomWithDefaults frequency = ftom frequency (i# 0)

instance Cps KRateSignal
instance Cps IRateSignal
