module Schisma.Csound.Opcodes.EnvelopeGenerators
  ( EnvelopeGenerator(..)
  , adsr140
  , adsrForTiedNotes
  ) where

import           Schisma.Csound.Opcodes.Initializations
                                                ( tieStatus )
import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , i#
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types

class (SignalGenerator a) => EnvelopeGenerator a where
  -- | Calculates the classical ADSR envelope using linear segments.
  --
  --   The envelope generated is the range 0 to 1 and may need to be scaled
  --   further, depending on the amplitude required. If using @0dbfs = 1@,
  --   scaling down will probably be required since playing more than one note
  --   might result in clipping. If not using @0dbfs@, scaling to a large
  --   amplitude (e.g. 32000) might be required.
  --
  --   The length of the sustain is calculated from the length of the note.
  --   This means 'adsr' is not suitable for use with MIDI events. The opcode
  --   'madsr' uses the 'Schisma.Csound.Opcodes.LinearGenerators.linsegr'
  --   mechanism, and so can be used in MIDI applications.
  --
  --   <https://csound.com/docs/manual/adsr.html Csound documentation>
  adsr
    :: IRateSignal -- ^ @attack@ - The duration of the attack phase.
    -> IRateSignal -- ^ @decay@ - The duration of the decay phase.
    -> IRateSignal -- ^ @sustain@ - The level for the sustain phase.
    -> IRateSignal -- ^ @release@ - The duration of the release phase.
    -> IRateSignal -- ^ @delay@ - The period of zero before the envelope
                   --   starts.
    -> a           -- ^ The returned signal.
  adsr attack decay sustain release delay =
    makeOpcodeSignal "adsr" $
      map getSignal [attack, decay, sustain, release, decay]

  -- | 'adsrWithDefaults' is identical to 'adsr' with a default value supplied
  --   for @delay@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/adsr.html Csound documentation>
  adsrWithDefaults
    :: IRateSignal -- ^ @attack@ - The duration of the attack phase.
    -> IRateSignal -- ^ @decay@ - The duration of the decay phase.
    -> IRateSignal -- ^ @sustain@ - The level for the sustain phase.
    -> IRateSignal -- ^ @release@ - The duration of the release phase.
    -> a           -- ^ The returned signal.
  adsrWithDefaults attack decay sustain release =
    adsr attack decay sustain release (i# 0)

  -- | Calculates the classical ADSR envelope using the
  --   'Schisma.Csound.Opcodes.LinearGenerators.linsegr' mechanism.
  --
  --   The envelope is in the range 0 to 1 and may need to be scaled further.
  --
  --   The length of the sustain is calculated from the length of the note.
  --
  --   <https://csound.com/docs/manual/madsr.html Csound documentation>
  madsr
    :: IRateSignal -- ^ @attack@ - The duration of the attack phase.
    -> IRateSignal -- ^ @decay@ - The duration of the decay phase.
    -> IRateSignal -- ^ @sustain@ - The level for the sustain phase.
    -> IRateSignal -- ^ @release@ - The duration of the release phase.
    -> IRateSignal -- ^ @delay@ - The period of zero before the envelope
                   --   starts.
    -> IRateSignal -- ^ @controlRelease@ - Control release time after receiving
                   --   a MIDI noteoff event. If less than zero, the longest
                   --   release time given in the current instrument is used.
                   --   If zero or more, the given value will be used for
                   --   release time.
    -> a           -- ^ The returned signal.
  madsr attack decay sustain release delay controlRelease =
    makeOpcodeSignal "madsr" $
      map getSignal [attack, decay, sustain, release, delay, controlRelease]

  -- | 'madsrWithDefaults' is identical to 'adsr' with default values supplied
  --   for @delay@ (@'i#' 0@) and @controlRelease@ (@'i#' (-1)@).
  --
  --   <https://csound.com/docs/manual/madsr.html Csound documentation>
  madsrWithDefaults
    :: IRateSignal -- ^ @attack@ - The duration of the attack phase.
    -> IRateSignal -- ^ @decay@ - The duration of the decay phase.
    -> IRateSignal -- ^ @sustain@ - The level for the sustain phase.
    -> IRateSignal -- ^ @release@ - The duration of the release phase.
    -> a           -- ^ The returned signal.
  madsrWithDefaults attack decay sustain release =
    madsr attack decay sustain release (i# 0) (i# (-1))

instance EnvelopeGenerator ARateSignal
instance EnvelopeGenerator KRateSignal


-- | A gated, retriggerable envelope generator UDO.
--
--   Based on the design of Doepfer A-140 envelope generator module.
adsr140
  :: ARateSignal -- ^ @gate@ - The gate. Values > 0 put the gate in an "on"
                 --   state.
  -> ARateSignal -- ^ @retrigger@ - A retrigger signal. Values > 0 reset the
                 --   state of the envelope back to the attack component.
  -> KRateSignal -- ^ @attack@ - The attack.
  -> KRateSignal -- ^ @decay@ - The decay.
  -> KRateSignal -- ^ @sustain@ - The sustain.
  -> KRateSignal -- ^ @release@ - The release.
  -> ARateSignal -- ^ The returned signal.
adsr140 gate retrigger attack decay sustain release = ARateSignal signal where
  args =
    [ getSignal gate
    , getSignal retrigger
    , getSignal attack
    , getSignal decay
    , getSignal sustain
    , getSignal release
    ]
  opcode = IncludedOpcode "adsr140" args [ARate]
  signal = Signal opcode 1

-- | Calculates the classical ADSR envelope using linear segments while
--   supporting tied notes.
adsrForTiedNotes
  :: IRateSignal -- ^ @attack@ - The attack.
  -> IRateSignal -- ^ @decay@ - The decay.
  -> IRateSignal -- ^ @sustain@ - The sustain.
  -> IRateSignal -- ^ @release@ - The release.
  -> KRateSignal -- ^ The returned signal.
adsrForTiedNotes attack decay sustain release = KRateSignal signal where
  args   = map getSignal [attack, decay, sustain, release]
  opcode = IncludedOpcode "adsrForTiedNotes" args [KRate]
  signal = Signal opcode 1
