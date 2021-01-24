module Schisma.Csound.Opcodes.InstrumentDuration
  ( turnoff2
  ) where

import           Schisma.Csound.Types

-- | Turn off instance(s) of other instruments at performance time.
--
--   As a rule of thumb, you should turn off instruments with a higher
--   instrument number than the one where turnoff is called, as doing
--   otherwise might cause initialization issues.
--
--   <https://csound.com/docs/manual/turnoff2.html Csound documentation>
turnoff2
  :: KRateSignal     -- ^ @instrumentNumber@ - The instrument to be turned off
                     --   (can be fractional). If zero or negative, no
                     --   instrument is turned off.
  -> KRateSignal     -- ^ @mode@ - The sum of the following values:
                     --
                     --   * 0, 1, or 2: turn off all instances (0), oldest
                     --     only (1), or newest only (2)
                     --   * 4: only turn off notes with exactly matching
                     --     (fractional) instrument number, rather than
                     --     ignoring fractional part
                     --   * 8: only turn off notes with indefinite duration
                     --     (@p3 < 0@ or MIDI)
  -> KRateSignal     -- ^ @release@ - If non-zero, the turned off instances are
                     --   allowed to release. Otherwise, they are deactivated
                     --   immediately (possibly resulting in clicks).
  -> StatementOpcode -- ^ The returned statement opcode.
turnoff2 instrumentNumber mode release = StatementOpcode "turnoff2" args
  where args = map getSignal [instrumentNumber, mode, release]
