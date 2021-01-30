module Schisma.Csound.Opcodes.SignalOutput where

import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , IsSignal(getSignal)
                                                , StatementOpcode(..)
                                                )

-- | Writes audio data to an external device or stream.
--
--   Sends audio samples to an accumulating output buffer (created at the
--   beginning of performance) which serves to collect the output of all
--   active instruments before the sound is written to disk. There can be
--   any number of these output units in an instrument.
--
--   <https://csound.com/docs/manual/out.html Csound documentation>
out
  :: [ARateSignal]   -- ^ @signals@ - The signals to output.
  -> StatementOpcode -- ^ The returned statement opcode.
out signals = StatementOpcode "out" (map getSignal signals)

-- | Writes stereo audio data to an external device or stream.
--
--   Sends stereo audio samples to an accumulating output buffer (created
--   at the beginning of performance) which serves to collect the output of
--   all active instruments before the sound is written to disk. There can
--   be any number of these output units in an instrument.
--
--   <https://csound.com/docs/manual/outs.html Csound documentation>
outs
  :: ARateSignal     -- ^ @signal1@ - The signal to be output to channel 1.
  -> ARateSignal     -- ^ @signal2@ - The signal to be output to channel 2.
  -> StatementOpcode -- ^ The returned statement opcode.
outs signal1 signal2 =
  StatementOpcode "outs" $ map getSignal [signal1, signal2]
