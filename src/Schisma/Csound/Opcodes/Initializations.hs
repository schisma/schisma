module Schisma.Csound.Opcodes.Initializations
  ( Assignment(..)
  , Initialization(..)
  , tieStatus
  , tival
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types

class (IsSignal a, SignalGenerator b) => Assignment a b where
  -- | Performs a simple assignment.
  --
  --  <https://csound.com/docs/manual/assign.html Csound documentation>
  assign
    :: a -- ^ @x@ - The signal to be assigned.
    -> b -- ^ The returned signal.
  assign x = makeOpcodeSignal "=" [getSignal x]

instance Assignment ARateSignal ARateSignal
instance Assignment KRateSignal ARateSignal
instance Assignment IRateSignal ARateSignal
instance Assignment KRateSignal KRateSignal
instance Assignment IRateSignal IRateSignal


class (SignalGenerator a) => Initialization a where
  -- | Puts the value of i-time @x@ into a k- or a-rate variable.
  --
  --   Note that 'init' provides the only case of an init-time statement
  --   being permitted to write into a perf-time (k- or a-rate) result
  --   cell; the statement has no effect at perf-time.
  --
  --  <https://csound.com/docs/manual/init.html Csound documentation>
  init
    :: IRateSignal -- ^ @x@ - The signal to be initialized.
    -> a           -- ^ The returned signal.
  init x = makeOpcodeSignal "init" [getSignal x]

instance Initialization ARateSignal
instance Initialization KRateSignal
instance Initialization IRateSignal


-- | Determines the tie status of a note. The possible values of the returned
--   signal are as follows:
--
--   * -1: This note is a free-standing note and is neither tied from or
--         tied to any other notes.
--   * 0:  This note is the first note in a group of tied notes. This note
--         is tied to the next note but is not tied from any other note.
--   * 1:  This note is contained within a group of tied notes and is
--         neither the initial or ending note of the group. This note is both
--         tied to the next note and tied from previous note.
--   * 2:  This note is the final note in a group of tied notes.  This not
--         is tied from a previous note but is not tied to any other notes.
--
--   A note with a duration of -1,000,000,000 will have a tie status of -1.
--
--   <http://www.csounds.com/journal/2005fall/tiedNotes.html Exploring Tied Notes in Csound>
tieStatus :: IRateSignal -- ^ The returned signal.
tieStatus = IRateSignal $ Signal opcode 1
  where opcode = IncludedOpcode "tieStatus" [] [IRate]

-- | Determines the value of the instrument's internal "tie-in" flag.
--
--   The value is 1 if this note has been "tied" onto a previously held note
--   or 0 if no tie actually took place.
--
--   <https://csound.com/docs/manual/tival.html Csound documentation>
tival :: IRateSignal -- ^ The returned signal.
tival = makeOpcodeSignal "tival" []
