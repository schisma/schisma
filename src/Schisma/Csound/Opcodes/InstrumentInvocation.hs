module Schisma.Csound.Opcodes.InstrumentInvocation
  ( Event(..)
  ) where

import           Schisma.Csound.Types

class (IsSignal a) => Event a where
  -- | Generates a score event from an instrument.
  --
  --   <https://csound.com/docs/manual/event.html Csound documentation>
  event
    :: SRateSignal     -- ^ @scoreCharacter@ - A character representing the
                       --   first p-field in a score statement. This is usually
                       --   "e", "f", or "i".
    -> a               -- ^ @instrumentNumber@ - The instrument to use for
                       --   the event. This corresponds to the first p-field,
                       --   p1, in a score statement.
    -> a               -- ^ @delay@ - When (in seconds) the event will occur
                       --   from the current performance time. This corresponds
                       --   to the second p-field, p2, in a score statement.
    -> a               -- ^ @duration@ - How long (in seconds) the event will
                       --   happen. This corresponds to the third p-field, p3,
                       --   in a score statement.
    -> [a]             -- ^ @parameters@ - Parameters representing additional
                       --   p-fields in a score statement, starting with the
                       --   fourth p-field, p4.
    -> StatementOpcode -- ^ The returned opcode.

instance Event KRateSignal where
  event scoreCharacter instrumentNumber delay duration parameters =
    StatementOpcode "event" args   where
    args = getSignal scoreCharacter
      : map getSignal ([instrumentNumber, delay, duration] ++ parameters)

instance Event IRateSignal where
  event scoreCharacter instrumentNumber delay duration parameters =
    StatementOpcode "event_i" args   where
    args = getSignal scoreCharacter
      : map getSignal ([instrumentNumber, delay, duration] ++ parameters)
