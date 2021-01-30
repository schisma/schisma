module Schisma.Csound.Opcodes.MidiEventExtenders
  ( xtratim
  ) where

import           Schisma.Csound.Types.Signals   ( IRateSignal
                                                , IsSignal(getSignal)
                                                , StatementOpcode(..)
                                                )

-- | Extend the duration of real-time generated events.
--
--   'xtratim' extends current MIDI-activated note duration by @duration@
--   seconds after the corresponding noteoff message has deactivated the
--   current note itself.
--
--   This opcode is useful for implementing complex release-oriented
--   envelopes, whose duration is not known when the envelope starts (e.g.,
--   for real-time MIDI generated events).
--
--   <https://csound.com/docs/manual/xtratim.html Csound documentation>
xtratim
  :: IRateSignal     -- ^ @duration@ - The additional duration of the current
                     --   instrument instance.
  -> StatementOpcode -- ^ The returned opcode.
xtratim duration = StatementOpcode "xtratim" [getSignal duration]
