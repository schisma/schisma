module Schisma.Csound.Opcodes.MidiInput
  ( Ctrl7(..)
  , isMidiNotePlaying
  , midiChannelMatches
  , midiin
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , i#
                                                , makeOpcodeSignal
                                                , makeOpcodeSignals
                                                )
import           Schisma.Csound.Types.Signals   ( IRateSignal
                                                , IsSignal(..)
                                                , KRateSignal(..)
                                                , Opcode(IncludedOpcode)
                                                , Signal(Signal)
                                                , SignalRate(KRate)
                                                )

class (IsSignal a, SignalGenerator a) => Ctrl7 a where
  -- | Allows a floating-point 7-bit MIDI signal scaled with a minimum and
  --   a maximum range.
  --
  --   It also allows optional non-interpolated table indexing.
  --
  --   Note that 'ctrl7' only outputs k values once the controller is first
  --   moved. To set an initial k-value, a call to 'initc7' is required.
  --
  --   <https://csound.com/docs/manual/ctrl7.html Csound documentation>
  ctrl7
    :: IRateSignal -- ^ @channel@ - The MIDI channel number (1-16).
    -> IRateSignal -- ^ @controller@ - The MIDI controller number (0-127).
    -> a           -- ^ @minimum@ - The minimum value after scaling.
    -> a           -- ^ @maximum@ - The maximum value after scaling.
    -> IRateSignal -- ^ @ftn@ - The table to be read when indexing is required.
                   --   The table must be normalized.
    -> a           -- ^ The returned signal.
  ctrl7 channel controller minimum maximum ftn = makeOpcodeSignal "ctrl7" args where
    args =  [ getSignal channel
            , getSignal controller
            , getSignal minimum
            , getSignal maximum
            , getSignal ftn
            ]

  -- | 'ctrl7WithDefaults' is identical to 'ctrl7' with a default value
  --   supplied for for @ftn@ (@'i#' (-1)@).
  --
  --   <https://csound.com/docs/manual/ctrl7.html Csound documentation>
  ctrl7WithDefaults
    :: IRateSignal -- ^ @channel@ - The MIDI controller number.
    -> IRateSignal -- ^ @controller@ - The MIDI controller number.
    -> a           -- ^ @minimum@ - The MIDI controller number.
    -> a           -- ^ @maximum@ - The MIDI controller number.
    -> a           -- ^ The returned signal.
  ctrl7WithDefaults channel controller minimum maximum =
    ctrl7 channel controller minimum maximum (i# (-1))

instance Ctrl7 KRateSignal
instance Ctrl7 IRateSignal


-- | Determines if a MIDI note-on or note-off is occurring on the same MIDI
--   channel as @channel@.
--
--   Two signals are returned. The first signal indicates the note status
--   (@1@ for note-on, @-1@ for note-off, @0@ otherwise), and the second
--   signal indicates the MIDI note number (or @-1@ if the note status is
--   @0@).
isMidiNotePlaying
  :: KRateSignal   -- ^ @channel@ - The channel to check.
  -> [KRateSignal] -- ^ The returned signals.
isMidiNotePlaying channel = map toSignal [1, 2] where
  args     = [getSignal channel]
  opcode   = IncludedOpcode "isMidiNotePlaying" args [KRate, KRate]
  toSignal = KRateSignal . Signal opcode

-- | Determines if the incoming MIDI data is sent on the same channel as
--   @channel@. Returns @1@ if @channel@ matches and @0@ if it doesn't.
midiChannelMatches
  :: KRateSignal -- ^ @channel@ - The channel to check.
  -> KRateSignal -- ^ The returned signal.
midiChannelMatches channel = KRateSignal signal where
  args   = [getSignal channel]
  opcode = IncludedOpcode "midiChannelMatches" args [KRate]
  signal = Signal opcode 1

-- | Returns a generic MIDI message received by the MIDI IN port.
--
--   'midiin' has no input arguments because it reads at the MIDI IN port
--   implicitly. Normally (i.e., when no messages are pending) the @status@
--   is zero. Only when MIDI data are present in the MIDI IN buffer is
--   @status@ set to the type of the relevant messages.
--
--   Be careful when using 'midiin' in low numbered instruments, since a MIDI
--   note will launch additional instances of the instrument, resulting in
--   duplicate events and weird behaviour.
--
--   <https://csound.com/docs/manual/midiin.html Csound documentation>
midiin :: [KRateSignal] -- ^ The returned signals. The first signal is the
                        --   @status@, which reflects the type of MIDI
                        --   message. It can be one of the following:
                        --
                        --   * 128 (note off)
                        --   * 144 (note on)
                        --   * 160 (polyphonic aftertouch)
                        --   * 176 (control change)
                        --   * 192 (program change)
                        --   * 208 (channel aftertouch)
                        --   * 224 (pitch bend)
                        --   * 0 if no MIDI message are pending in the MIDI
                        --     IN buffer
                        --
                        --   The second signal is the MIDI channel (1-16).
                        --
                        --   The third and fourth signals contain values
                        --   that are based on the specific MIDI message.
midiin = makeOpcodeSignals "midiin" [] 4
