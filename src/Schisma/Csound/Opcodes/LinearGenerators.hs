module Schisma.Csound.Opcodes.LinearGenerators
  ( LinearGenerator(..)
  , scale
  , scaleWithDefaults
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , k#
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , IRateSignal
                                                , IsSignal(getSignal)
                                                , KRateSignal
                                                )

import           Schisma.Utilities              ( interleave )

class (SignalGenerator a) => LinearGenerator a where
  -- | Trace a straight line between specified points.
  --
  --   'line' generates control or audio signals whose values move linearly
  --   from an initial value to a final one.
  --
  --   A common error with this opcode is to assume that the value of @end@ is
  --   held after the time @duration@. 'line' does not automatically end or
  --   stop at the end of the duration given. If your note length is longer
  --   than @duration@ seconds, the final value will not come to rest at @end@,
  --   but will instead continue to rise or fall with the same rate. If a rise
  --   (or fall) and then hold is required, then the 'linseg' opcode should be
  --   considered instead.
  --
  --   <https://csound.com/docs/manual/line.html Csound documentation>
  line
    :: IRateSignal -- ^ @start@ - The starting value.
    -> IRateSignal -- ^ @duration@ - The duration in seconds of the segment. A
                   --   zero or negative value will cause all initialization to
                   --   be skipped.
    -> IRateSignal -- ^ @end@ - The ending value.
    -> a           -- ^ The returned signal.
  line start duration end =
    makeOpcodeSignal "line" $ map getSignal [start, duration, end]

  -- | Trace a series of line segments between specified points.
  --
  --   These units generate control or audio signals whose values can pass
  --   through 2 or more specified points. The sum of @durations@ may or may
  --   not equal the instrument's performance time; a shorter performance
  --   will truncate the specified pattern, while a longer one will cause
  --   the last value to be repeated until the end of the note.
  --
  --   <https://csound.com/docs/manual/linseg.html Csound documentation>
  linseg
    :: [IRateSignal] -- ^ @values@ - The values.
    -> [IRateSignal] -- ^ @durations@ - The duration (in seconds) of each
                     --   segment. A zero or negative value will terminate
                     --   the initialization process with the preceding
                     --   point, permitting the last-defined line to be
                     --   continued indefinitely in performance.
    -> a             -- ^ The returned signal.
  linseg values durations =
    makeOpcodeSignal "linseg" $ map getSignal (interleave values durations)

  -- | Trace a series of line segments between specified points including
  --   a release segment.
  --
  --   These units generate control or audio signals whose values can pass
  --   through 2 or more specified points. The sum of @durations@ may or may
  --   not equal the instrument's performance time; a shorter performance
  --   will truncate the specified pattern, while a longer one will cause
  --   the last value to be repeated until the end of the note.
  --
  --   'linsegr' is amongst the Csound "r" units that contain a note-off
  --   sensor and release time extender. When each senses an event
  --   termination or MIDI noteoff, it immediately extends the performance
  --   time of the current instrument by @release@ seconds, and sets out to
  --   reach the value @final@ by the end of that period (no matter which
  --   segment the unit is in). "r" units can also be modified by MIDI
  --   noteoff velocities. For two or more extenders in an instrument,
  --   extension is by the greatest period.
  --
  --   Note that you don't need to use
  --   'Schisma.Csound.Opcodes.MidiEventExtenders.xtratim' if you are using
  --   'linsegr', since the time is extended automatically.
  --
  --   <https://csound.com/docs/manual/linsegr.html Csound documentation>
  linsegr
    :: [IRateSignal] -- ^ @values@ - The values.
    -> [IRateSignal] -- ^ @durations@ - The duration (in seconds) of each
                     --   segment. A zero or negative value will terminate
                     --   the initialization process with the preceding
                     --   point, permitting the last-defined line to be
                     --   continued indefinitely in performance.
    -> IRateSignal   -- ^ @release@ - The duration (in seconds) of a note
                     --   releasing segment.
    -> IRateSignal   -- ^ @final@ - The final value of a note releasing
                     --   segment.
    -> a             -- ^ The returned signal.
  linsegr values durations release final =
    makeOpcodeSignal "linsegr" $
      map getSignal (interleave values durations ++ [release, final])

instance LinearGenerator ARateSignal
instance LinearGenerator KRateSignal


-- | Arbitrary signal scaling.
--
--   Scales incoming value to user-definable range. Similar to scale object
--   found in popular dataflow languages.
--
--   <https://csound.com/docs/manual/scale.html Csound documentation>
scale
  :: KRateSignal -- ^ @input@ -  The input value. Can originate from any k-rate
                 --   source as long as that source's output is in range 0-1.
  -> KRateSignal -- ^ @resultantMin@ - The minimum value of the resultant scale
                 --   operation.
  -> KRateSignal -- ^ @resultantMax@ - The maximum value of the resultant scale
                 --   operation.
  -> KRateSignal -- ^ @incomingMin@ - The minimum value of @input@.
  -> KRateSignal -- ^ @incomingMax@ - The maximum value of @input@.
  -> KRateSignal -- ^ The returned signal.
scale input resultantMin resultantMax incomingMin incomingMax =
  makeOpcodeSignal "scale" $ map
    getSignal
    [input, resultantMax, resultantMin, incomingMax, incomingMin]

-- | 'scaleWithDefaults' is identical to 'scale' with default values
--   supplied for @incomingMin@ (@'k#' 0@) and @incomingMax@ (@'k#' 1@).
--
--   <https://csound.com/docs/manual/scale.html Csound documentation>
scaleWithDefaults
  :: KRateSignal -- ^ @input@ -  The input value. Can originate from any k-rate
                 --   source as long as that source's output is in range 0-1.
  -> KRateSignal -- ^ @resultantMin@ - The minimum value of the resultant scale
                 --   operation.
  -> KRateSignal -- ^ @resultantMax@ - The maximum value of the resultant scale
                 --   operation.
  -> KRateSignal -- ^ The returned signal.
scaleWithDefaults input resultantMin resultantMax =
  scale input resultantMin resultantMax (k# 0) (k# 1)
