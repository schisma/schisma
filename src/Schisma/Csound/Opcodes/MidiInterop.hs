module Schisma.Csound.Opcodes.MidiInterop
  ( MidiInteroperability(..)
  )
where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator, i# )
import           Schisma.Csound.Types

class (SignalGenerator a, SignalGenerator b) => MidiInteroperability a b where
  -- | Gets a MIDI control change value.
  --
  --   If the instrument was activated by MIDI input, this opcode overwrites
  --   the values of @controller@ and @value@ with the corresponding
  --   values from MIDI input. If the instrument was NOT activated by MIDI
  --   input, the values of @controller@ and @value@ remain unchanged.
  --
  --   This enables score p-fields to receive MIDI input data during MIDI
  --   activation, and score values otherwise.
  --
  --   <https://csound.com/docs/manual/midicontrolchange.html Csound documentation>
  midicontrolchange
    :: a           -- ^ @controller@ - The MIDI controller number.
    -> b           -- ^ @value@ - The value of the MIDI controller.
    -> IRateSignal -- ^ @low@ - The low value after rescaling.
    -> IRateSignal -- ^ @high@ - The high value after rescaling.
    -> (a, b)      -- ^ The returned signals.

  -- | 'midicontrolchangeWithDefaults' is identical to 'midicontrolchange'
  --   with default values supplied for @low@ (@'i#' 0@), and
  --   @high@ (@'i#' 127@).
  --
  --   <https://csound.com/docs/manual/midicontrolchange.html Csound documentation>
  midicontrolchangeWithDefaults
    :: a           -- ^ @controller@ - The MIDI controller number.
    -> b           -- ^ @value@ - The value of the MIDI controller.
    -> (a, b)      -- ^ The returned signals.

  -- | Gets a MIDI note number as a cycles-per-second frequency.
  --
  --   If the instrument was activated by MIDI input, this opcode overwrites
  --   the values of @frequency@ and @velocity@ with the corresponding
  --   values from MIDI input. If the instrument was NOT activated by MIDI
  --   input, the values of @frequency@ and @velocity@ remain unchanged.
  --
  --   This enables score p-fields to receive MIDI input data during MIDI
  --   activation, and score values otherwise.
  --
  --   <https://csound.com/docs/manual/midinoteoncps.html Csound documentation>
  midinoteoncps
    :: a      -- ^ @frequency@ - The MIDI key translated to frequency in cycles
              --   per second.
    -> b      -- ^ @velocity@ - The MIDI velocity.
    -> (a, b) -- ^ The returned signals.

instance MidiInteroperability ARateSignal ARateSignal where
  midicontrolchange controller value low high =
    (ARateSignal $ Signal opcode 1, ARateSignal $ Signal opcode 2)
   where
    args   = [getSignal controller, getSignal value, getSignal low, getSignal high]
    opcode = MutatingOpcode "midicontrolchange" args [0, 1]

  midicontrolchangeWithDefaults controller value =
    midicontrolchange controller value (i# 0) (i# 127)

  midinoteoncps frequency velocity =
    (ARateSignal $ Signal opcode 1, ARateSignal $ Signal opcode 2)
   where
    args   = map getSignal [frequency, velocity]
    opcode = MutatingOpcode "midinoteoncps" args [0, 1]

instance MidiInteroperability ARateSignal KRateSignal where
  midicontrolchange controller value low high =
    (ARateSignal $ Signal opcode 1, KRateSignal $ Signal opcode 2)
   where
    args   = [getSignal controller, getSignal value, getSignal low, getSignal high]
    opcode = MutatingOpcode "midicontrolchange" args [0, 1]

  midicontrolchangeWithDefaults controller value =
    midicontrolchange controller value (i# 0) (i# 127)

  midinoteoncps frequency velocity =
    (ARateSignal $ Signal opcode 1, KRateSignal $ Signal opcode 2)
   where
    args   = [getSignal frequency, getSignal velocity]
    opcode = MutatingOpcode "midinoteoncps" args [0, 1]

instance MidiInteroperability ARateSignal IRateSignal where
  midicontrolchange controller value low high =
    (ARateSignal $ Signal opcode 1, IRateSignal $ Signal opcode 2)
   where
    args   = [getSignal controller, getSignal value, getSignal low, getSignal high]
    opcode = MutatingOpcode "midicontrolchange" args [0, 1]

  midicontrolchangeWithDefaults controller value =
    midicontrolchange controller value (i# 0) (i# 127)

  midinoteoncps frequency velocity =
    (ARateSignal $ Signal opcode 1, IRateSignal $ Signal opcode 2)
   where
    args   = [getSignal frequency, getSignal velocity]
    opcode = MutatingOpcode "midinoteoncps" args [0, 1]

instance MidiInteroperability KRateSignal ARateSignal where
  midicontrolchange controller value low high =
    (KRateSignal $ Signal opcode 1, ARateSignal $ Signal opcode 2)
   where
    args   = [getSignal controller, getSignal value, getSignal low, getSignal high]
    opcode = MutatingOpcode "midicontrolchange" args [0, 1]

  midicontrolchangeWithDefaults controller value =
    midicontrolchange controller value (i# 0) (i# 127)

  midinoteoncps frequency velocity =
    (KRateSignal $ Signal opcode 1, ARateSignal $ Signal opcode 2)
   where
    args   = [getSignal frequency, getSignal velocity]
    opcode = MutatingOpcode "midinoteoncps" args [0, 1]

instance MidiInteroperability KRateSignal KRateSignal where
  midicontrolchange controller value low high =
    (KRateSignal $ Signal opcode 1, KRateSignal $ Signal opcode 2)
   where
    args   = [getSignal controller, getSignal value, getSignal low, getSignal high]
    opcode = MutatingOpcode "midicontrolchange" args [0, 1]

  midicontrolchangeWithDefaults controller value =
    midicontrolchange controller value (i# 0) (i# 127)

  midinoteoncps frequency velocity =
    (KRateSignal $ Signal opcode 1, KRateSignal $ Signal opcode 2)
   where
    args   = map getSignal [frequency, velocity]
    opcode = MutatingOpcode "midinoteoncps" args [0, 1]

instance MidiInteroperability KRateSignal IRateSignal where
  midicontrolchange controller value low high =
    (KRateSignal $ Signal opcode 1, IRateSignal $ Signal opcode 2)
   where
    args   = [getSignal controller, getSignal value, getSignal low, getSignal high]
    opcode = MutatingOpcode "midicontrolchange" args [0, 1]

  midicontrolchangeWithDefaults controller value =
    midicontrolchange controller value (i# 0) (i# 127)

  midinoteoncps frequency velocity =
    (KRateSignal $ Signal opcode 1, IRateSignal $ Signal opcode 2)
   where
    args   = [getSignal frequency, getSignal velocity]
    opcode = MutatingOpcode "midinoteoncps" args [0, 1]

instance MidiInteroperability IRateSignal ARateSignal where
  midicontrolchange controller value low high =
    (IRateSignal $ Signal opcode 1, ARateSignal $ Signal opcode 2)
   where
    args   = [getSignal controller, getSignal value, getSignal low, getSignal high]
    opcode = MutatingOpcode "midicontrolchange" args [0, 1]

  midicontrolchangeWithDefaults controller value =
    midicontrolchange controller value (i# 0) (i# 127)

  midinoteoncps frequency velocity =
    (IRateSignal $ Signal opcode 1, ARateSignal $ Signal opcode 2)
   where
    args   = [getSignal frequency, getSignal velocity]
    opcode = MutatingOpcode "midinoteoncps" args [0, 1]

instance MidiInteroperability IRateSignal KRateSignal where
  midicontrolchange controller value low high =
    (IRateSignal $ Signal opcode 1, KRateSignal $ Signal opcode 2)
   where
    args   = [getSignal controller, getSignal value, getSignal low, getSignal high]
    opcode = MutatingOpcode "midicontrolchange" args [0, 1]

  midicontrolchangeWithDefaults controller value =
    midicontrolchange controller value (i# 0) (i# 127)

  midinoteoncps frequency velocity =
    (IRateSignal $ Signal opcode 1, KRateSignal $ Signal opcode 2)
   where
    args   = [getSignal frequency, getSignal velocity]
    opcode = MutatingOpcode "midinoteoncps" args [0, 1]

instance MidiInteroperability IRateSignal IRateSignal where
  midicontrolchange controller value low high =
    (IRateSignal $ Signal opcode 1, IRateSignal $ Signal opcode 2)
   where
    args   = [getSignal controller, getSignal value, getSignal low, getSignal high]
    opcode = MutatingOpcode "midicontrolchange" args [0, 1]

  midicontrolchangeWithDefaults controller value =
    midicontrolchange controller value (i# 0) (i# 127)

  midinoteoncps frequency velocity =
    (IRateSignal $ Signal opcode 1, IRateSignal $ Signal opcode 2)
   where
    args   = map getSignal [frequency, velocity]
    opcode = MutatingOpcode "midinoteoncps" args [0, 1]
