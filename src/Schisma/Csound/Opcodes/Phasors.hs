module Schisma.Csound.Opcodes.Phasors
  ( Phasor(..)
  , SyncPhasor(..)
  )
where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , i#
                                                , makeOpcodeSignal
                                                , makeOpcodeSignals
                                                )
import           Schisma.Csound.Types

class (IsSignal a, SignalGenerator b) => Phasor a b where
  -- | Produce a normalized moving phase value.
  --
  --   An internal phase is successively accumulated in accordance with the
  --   @frequency@ to produce a moving phase value, normalized
  --   to lie in the range @0 <= phase < 1@.
  --
  --   When used as the index to a 'table' unit, this phase (multiplied by
  --   the desired function table length) will cause it to behave like an
  --   oscillator.
  --
  --   Note that 'phasor' is a special kind of integrator, accumulating phase
  --   increments that represent frequency settings.
  --
  --   <https://csound.com/docs/manual/phasor.html Csound documentation>
  phasor
    :: a           -- ^ @frequency@ - The frequency in cycles per second.
    -> IRateSignal -- ^ @phase@ - The initial phase, expressed as a fraction of
                   --   a cycle (0 to 1). A negative value will cause phase
                   --   initialization to be skipped.
    -> b           -- ^ The returned signal.
  phasor frequency phase = makeOpcodeSignal "phasor" args where
    args = [getSignal frequency, getSignal phase]

  -- | 'phasorWithDefaults' is identical to 'phasor' with a default value
  --   supplied for @phase@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/phasor.html Csound documentation>
  phasorWithDefaults
    :: a           -- ^ @frequency@ - The frequency in cycles per second.
    -> b           -- ^ The returned signal.
  phasorWithDefaults frequency = phasor frequency (i# 0)

instance Phasor ARateSignal ARateSignal
instance Phasor KRateSignal ARateSignal
instance Phasor IRateSignal ARateSignal
instance Phasor KRateSignal KRateSignal


class (IsSignal a) => SyncPhasor a where
  -- | Produces a moving phase value between zero and one and an extra
  --   impulse output ("sync out") whenever its phase value crosses or is
  --   reset to zero. The phase can be reset at any time by an impulse on the
  --   @input@ parameter.
  --
  --   An internal phase is successively accumulated in accordance with
  --   @frequency@ to produce a moving phase value, normalized to
  --   lie in the range 0 <= phase < 1. When used as the index to a table
  --   unit, this phase (multiplied by the desired function table length)
  --   will cause it to behave like an oscillator.
  --
  --   The phase of 'syncphasor' though can be synced to another phasor (or
  --   other signal) using the @input@ parameter. Any time that @input@ is
  --   a non-zero value, the value of the output phase will be reset to zero.
  --   'syncphasor' also outputs its own "sync" signal that consists of
  --   a one-sample impulse whenever its phase crosses zero or is reset.
  --   This makes it easy to chain together multiple syncphasor opcodes to
  --   create an oscillator "hard sync" effect.
  --
  --   <https://csound.com/docs/manual/syncphasor.html Csound documentation>
  syncphasor
    :: a             -- ^ @frequency@ - The frequency of the phasor in
                     --   cycles-per-second. If this is negative, the phase
                     --   value will decrease from 1 to 0 instead of
                     --   increasing.
    -> ARateSignal   -- ^ @input@ - The sync input, which causes the phase to
                     --   reset to zero whenever @input@ is non-zero.
    -> IRateSignal   -- ^ @phase@ - The initial phase, expressed as a fraction
                     --   of a cycle (0 to 1). A negative value will cause
                     --   phase initialization to be skipped.
    -> [ARateSignal] -- ^ The returned signals. The first signal is the output
                     --   phase value (which is always between 0 and 1).
                     --   The second signal is the sync output, which has a
                     --   value of 1.0 for one sample whenever the phase value
                     --   crosses zero or whenever @input@ is non-zero. It
                     --   is zero at all other times.
  syncphasor frequency input phase = makeOpcodeSignals "syncphasor" args 2 where
    args = [getSignal frequency, getSignal input, getSignal phase]

  -- | 'syncphasorWithDefaults' is identical to 'syncphasor' with a default
  --   value supplied for @phase@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/syncphasor.html Csound documentation>
  syncphasorWithDefaults
    :: a             -- ^ @frequency@ - The frequency of the phasor in
                     --   cycles-per-second. If this is negative, the phase
                     --   value will decrease from 1 to 0 instead of
                     --   increasing.
    -> ARateSignal   -- ^ @input@ - The sync input, which causes the phase to
                     --   reset to zero whenever @input@ is non-zero.
    -> [ARateSignal] -- ^ The returned signals. The first signal is the output
                     --   phase value (which is always between 0 and 1).
                     --   The second signal is the sync output, which has a
                     --   value of 1.0 for one sample whenever the phase value
                     --   crosses zero or whenever @input@ is non-zero. It
                     --   is zero at all other times.
  syncphasorWithDefaults frequency input = syncphasor frequency input (i# 0)

instance SyncPhasor ARateSignal
instance SyncPhasor KRateSignal
instance SyncPhasor IRateSignal
