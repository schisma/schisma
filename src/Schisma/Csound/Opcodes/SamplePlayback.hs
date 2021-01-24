module Schisma.Csound.Opcodes.SamplePlayback
  ( SFPlayer(..)
  , fluidEngine
  , fluidEngineWithDefaults
  , fluidLoad
  , fluidLoadWithDefaults
  , fluidNote
  , fluidOut
  , fluidProgramSelect
  , sfload
  , sflooper
  , sflooperWithDefaults
  , sfpreset
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , makeOpcodeSignal
                                                , makeOpcodeSignals
                                                )
import           Schisma.Csound.Types

class (IsSignal a) => SFPlayer a where
  -- | Plays a SoundFont2 (SF2) sample preset, generating a stereo sound.
  --
  --   <https://csound.com/docs/manual/sfplay.html Csound documentation>
  sfplay
    :: IRateSignal   -- ^ @velocity@ - The MIDI velocity. @velocity@ does not
                     --   directly affect the amplitude of the output, but
                     --   informs 'sfplay' about which sample should be chosen
                     --   in multi-sample, velocity-split presets.
    -> IRateSignal   -- ^ @noteNumber@ - The MIDI note number.
    -> a             -- ^ @amplitudeCorrection@ - The amplitude correction
                     --   factor, which acts as a multiplier.
    -> a             -- ^ @frequency@ - The frequency value or multiplier,
                     --   depending on @flag@. When @flag@ is 0, @frequency@ is
                     --   a multiplier of the default frequency, assigned by
                     --   the SF2 preset to the @noteNumber@ value. When
                     --   @flag@ is 1, @frequency@ is the absolute frequency of
                     --   the output sound, in Hz.
    -> IRateSignal   -- ^ @identifier@ - The preset identifier. This must
                     --   contain the number of a previously assigned preset,
                     --   or Csound will crash.
    -> IRateSignal   -- ^ @flag@ - The flag regarding the behavior of
                     --   @frequency@ and @noteNumber@.
                     --
                     --   When @flag@ is 0, @noteNumber@ sets the frequency of
                     --   the output according to the MIDI note number used,
                     --   and @frequency@ is used as a multiplier. When @flag@
                     --   is 1, the frequency of the output is set directly by
                     --   @frequency@. This allows the user to use any kind of
                     --   micro-tuning based scales. However, this method is
                     --   designed to work correctly only with presets tuned to
                     --   the default equal temperament. Attempts to use this
                     --   method with a preset already having non-standard
                     --   tunings, or with drum-kit-based presets, could give
                     --   unexpected results.
    -> IRateSignal   -- ^ @offset@ - The offset from the start, in samples. Its
                     --   value should be within the length of the specific
                     --   sound. Otherwise, Csound will probably crash.
    -> IRateSignal   -- ^ @envelope@ - The amplitude envelope.
                     --
                     --   One may use any of the following values for
                     --   @envelope@:
                     --
                     --   * 0: no envelope
                     --   * 1: linear attack and decay
                     --   * 2: linear attack, exponential decay
    -> [ARateSignal] -- ^ The returned signals.
  sfplay velocity noteNumber amplitudeCorrection frequency identifier flag offset envelope =
    makeOpcodeSignals "sfplay" args 2 where
      args = [ getSignal velocity
             , getSignal noteNumber
             , getSignal amplitudeCorrection
             , getSignal frequency
             , getSignal identifier
             , getSignal flag
             , getSignal offset
             , getSignal envelope
             ]

  -- | 'sfplayWithDefaults' is identical to 'sfplay' with default values
  --   supplied for @flag@ (@'i#' 0@), @offset@ (@'i#' 0@), and
  --   @envelope@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/sfplay.html Csound documentation>
  sfplayWithDefaults
    :: IRateSignal   -- ^ @velocity@ - The MIDI velocity. @velocity@ does not
                     --   directly affect the amplitude of the output, but
                     --   informs 'sfplay' about which sample should be chosen
                     --   in multi-sample, velocity-split presets.
    -> IRateSignal   -- ^ @noteNumber@ - The MIDI note number.
    -> a             -- ^ @amplitudeCorrection@ - The amplitude correction
                     --   factor, which acts as a multiplier.
    -> a             -- ^ @frequency@ - The frequency value or multiplier,
                     --   depending on @flag@. When @flag@ is 0, @frequency@ is
                     --   a multiplier of the default frequency, assigned by
                     --   the SF2 preset to the @noteNumber@ value. When
                     --   @flag@ is 1, @frequency@ is the absolute frequency of
                     --   the output sound, in Hz.
    -> IRateSignal   -- ^ @identifier@ - The preset identifier. This must
                     --   contain the number of a previously assigned preset,
                     --   or Csound will crash.
    -> [ARateSignal] -- ^ The returned signals.
  sfplayWithDefaults velocity noteNumber amplitudeCorrection frequency identifier =
    sfplay velocity noteNumber amplitudeCorrection frequency identifier flag offset envelope where
      flag = i# 0
      offset = i# 0
      envelope = i# 0

  -- | Plays a SoundFont2 (SF2) sample preset, generating a stereo sound.
  --
  --   This opcode supports the use of tied notes, though any a-rate or
  --   k-rate arguments provided to @amplitudeCorrection@ or @frequency@
  --   must also support tied notes for playback to occur as intended.
  --
  --   <https://csound.com/docs/manual/sfplay.html Csound documentation>
  sfplayForTiedNotes
    :: IRateSignal   -- ^ @velocity@ - The MIDI velocity. @velocity@ does not
                     --   directly affect the amplitude of the output, but
                     --   informs 'sfplay' about which sample should be chosen
                     --   in multi-sample, velocity-split presets.
    -> IRateSignal   -- ^ @noteNumber@ - The MIDI note number.
    -> a             -- ^ @amplitudeCorrection@ - The amplitude correction
                     --   factor, which acts as a multiplier.
    -> a             -- ^ @frequency@ - The frequency value or multiplier,
                     --   depending on @flag@. When @flag@ is 0, @frequency@ is
                     --   a multiplier of the default frequency, assigned by
                     --   the SF2 preset to the @noteNumber@ value. When
                     --   @flag@ is 1, @frequency@ is the absolute frequency of
                     --   the output sound, in Hz.
    -> IRateSignal   -- ^ @identifier@ - The preset identifier. This must
                     --   contain the number of a previously assigned preset,
                     --   or Csound will crash.
    -> IRateSignal   -- ^ @flag@ - The flag regarding the behavior of
                     --   @frequency@ and @noteNumber@.
                     --
                     --   When @flag@ is 0, @noteNumber@ sets the frequency of
                     --   the output according to the MIDI note number used,
                     --   and @frequency@ is used as a multiplier. When @flag@
                     --   is 1, the frequency of the output is set directly by
                     --   @frequency@. This allows the user to use any kind of
                     --   micro-tuning based scales. However, this method is
                     --   designed to work correctly only with presets tuned to
                     --   the default equal temperament. Attempts to use this
                     --   method with a preset already having non-standard
                     --   tunings, or with drum-kit-based presets, could give
                     --   unexpected results.
    -> IRateSignal   -- ^ @offset@ - The offset from the start, in samples. Its
                     --   value should be within the length of the specific
                     --   sound. Otherwise, Csound will probably crash.
    -> IRateSignal   -- ^ @envelope@ - The amplitude envelope.
                     --
                     --   One may use any of the following values for
                     --   @envelope@:
                     --
                     --   * 0: no envelope
                     --   * 1: linear attack and decay
                     --   * 2: linear attack, exponential decay
    -> [ARateSignal] -- ^ The returned signals.
  sfplayForTiedNotes velocity noteNumber amplitudeCorrection frequency identifier flag offset envelope =
    map toSignal [1, 2] where
      args = [ getSignal velocity
             , getSignal noteNumber
             , getSignal amplitudeCorrection
             , getSignal frequency
             , getSignal identifier
             , getSignal flag
             , getSignal offset
             , getSignal envelope
             ]
      opcode   = IncludedOpcode "sfplayForTiedNotes" args [ARate, ARate]
      toSignal = ARateSignal . Signal opcode

  -- | 'sfplayForTiedNotesWithDefaults' is identical to
  --   'sfplayForTiedNotes' with default values supplied for @flag@
  --   (@'i#' 0@), @offset@ (@'i#' 0@), and @envelope@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/sfplay.html Csound documentation>
  sfplayForTiedNotesWithDefaults
    :: IRateSignal   -- ^ @velocity@ - The MIDI velocity. @velocity@ does not
                     --   directly affect the amplitude of the output, but
                     --   informs 'sfplay' about which sample should be chosen
                     --   in multi-sample, velocity-split presets.
    -> IRateSignal   -- ^ @noteNumber@ - The MIDI note number.
    -> a             -- ^ @amplitudeCorrection@ - The amplitude correction
                     --   factor, which acts as a multiplier.
    -> a             -- ^ @frequency@ - The frequency value or multiplier,
                     --   depending on @flag@. When @flag@ is 0, @frequency@ is
                     --   a multiplier of the default frequency, assigned by
                     --   the SF2 preset to the @noteNumber@ value. When
                     --   @flag@ is 1, @frequency@ is the absolute frequency of
                     --   the output sound, in Hz.
    -> IRateSignal   -- ^ @identifier@ - The preset identifier. This must
                     --   contain the number of a previously assigned preset,
                     --   or Csound will crash.
    -> [ARateSignal] -- ^ The returned signals.
  sfplayForTiedNotesWithDefaults velocity noteNumber amplitudeCorrection frequency identifier =
    sfplayForTiedNotes velocity noteNumber amplitudeCorrection frequency identifier flag offset envelope where
      flag = i# 0
      offset = i# 0
      envelope = i# 0

instance SFPlayer ARateSignal
instance SFPlayer KRateSignal
instance SFPlayer IRateSignal

-- | Instantiates a FluidSynth engine.
--
--   This opcode can be safely used within an instrument definition.
--
--   <https://csound.com/docs/manual/fluidEngine.html Csound documentation>
fluidEngine
  :: SRateSignal -- ^ @soundFont@ - The path to the SF2 file.
  -> IRateSignal -- ^ @chorus@ - Disable any chorus effect with @0@.
  -> IRateSignal -- ^ @reverb@ - Disable any reverb effect with @0@.
  -> IRateSignal -- ^ @channels@ - The number of channels to use. The range is
                 --   16-256 and the Csound default is 256 (FluidSynth's
                 --   default is 16).
  -> IRateSignal -- ^ @voices@ - The number of voices to be played in parallel.
                 --   The range is 16-4096 and the Csound default is 4096
                 --   (FluidSynth's default is 256). Note: this is not the
                 --   number of notes played at the same time, as a single
                 --   note may use/create multiple voices depending on
                 --   instrument zones and velocity/key of played note.
  -> IRateSignal -- ^ The FluidSynth engine number.
fluidEngine soundFont chorus reverb channels voices = IRateSignal signal where
  args = getSignal soundFont : map getSignal [chorus, reverb, channels, voices]
  opcode = IncludedOpcode "loadFluidEngine" args [IRate]
  signal = Signal opcode 1

-- | 'fluidEngineWithDefaults' is identical to 'fluidEngine' with
--   default values supplied for @chorus@ (@'i#' 0@), @reverb@ (@'i#' 0@),
--   @channels@ (@'i#' 256@), and @voices@ (@'i#' 4096@).
--
--   <https://csound.com/docs/manual/fluidEngine.html Csound documentation>
fluidEngineWithDefaults
  :: SRateSignal -- ^ @soundFont@ - The path to the SF2 file.
  -> IRateSignal -- ^ The FluidSynth engine number.
fluidEngineWithDefaults soundFont =
  fluidEngine soundFont (i# 0) (i# 0) (i# 256) (i# 4096)

-- | Loads a SoundFont into an instance of a FluidSynth engine and
--   optionally lists the banks and presets of the SoundFont.
--
--   The same SoundFont may be invoked to assign programs to MIDI channels
--   any number of times; the SoundFont is only loaded the first time.
--
--   <https://csound.com/docs/manual/fluidLoad.html Csound documentation>
fluidLoad
  :: SRateSignal -- ^ @soundFont@ - The path to the SF2 file.
  -> IRateSignal -- ^ @engine@ - The FluidSynth engine number (assigned from
                 --   'fluidEngine').
  -> IRateSignal -- ^ @listPresets@ - If @1@, lists all of the FluidSynth
                 --   programs for the @soundFont@. A FluidSynth program is
                 --   a combination of SoundFont ID, bank number, and
                 --   preset number that is assigned to a MIDI channel.
  -> IRateSignal -- ^ The SoundFont number within the FluidSynth.
fluidLoad soundFont engine listPresets = makeOpcodeSignal "fluidLoad" args
  where args = getSignal soundFont : map getSignal [engine, listPresets]

-- | 'fluidLoadWithDefaults' is identical to 'fluidLoad' with a default
--   value supplied for @listPresets@ (@'i#' 0@).
--
--   <https://csound.com/docs/manual/fluidLoad.html Csound documentation>
fluidLoadWithDefaults
  :: SRateSignal -- ^ @soundFont@ - The path to the SF2 file.
  -> IRateSignal -- ^ @engine@ - The FluidSynth engine number (assigned from
                 --   'fluidEngine').
  -> IRateSignal -- ^ The SoundFont number within the FluidSynth.
fluidLoadWithDefaults soundFont engine = fluidLoad soundFont engine (i# 0)

-- | Plays a note on a channel in a FluidSynth engine.
--
--   <https://csound.com/docs/manual/fluidNote.html Csound documentation>
fluidNote
  :: IRateSignal     -- ^ @engine@ - The FluidSynth engine number (assigned
                     --   from 'fluidEngine').
  -> IRateSignal     -- ^ @channel@ - The channel number on which to play a
                     --   note in the given FluidSynth engine.
  -> IRateSignal     -- ^ @midiKey@ - The MIDI key for the note (0-127).
  -> IRateSignal     -- ^ @midiVelocity@ - The MIDI velocity for the note
                     --   (0-127).
  -> StatementOpcode -- ^ The returned statement opcode.
fluidNote engine channel midiKey midiVelocity =
  StatementOpcode "fluidNote"
    $ map getSignal [engine, channel, midiKey, midiVelocity]

-- | Outputs sound from a FluidSynth engine.
--
--   'fluidOut' should be invoked in an instrument definition numbered
--   higher than any fluidcontrol instrument definitions. All SoundFonts
--   used in the FluidSynth engine numbered @engine@ send their audio
--   output to this one opcode. Send a note with an indefinite duration to
--   this instrument to turn the SoundFonts on for as long as required.
--
--   <https://csound.com/docs/manual/fluidOut.html Csound documentation>
fluidOut
  :: IRateSignal    -- ^ @engine@ - The FluidSynth engine number (assigned
                    --   from 'fluidEngine').
  -> [ARateSignal]  -- ^ The returned signals.
fluidOut engine = makeOpcodeSignals "fluidOut" [getSignal engine] 2

-- | Assigns a preset from a SoundFont to a channel on a FluidSynth engine.
--
--   <https://csound.com/docs/manual/fluidProgramSelect.html Csound documentation>
fluidProgramSelect
  :: IRateSignal     -- ^ @engine@ - The FluidSynth engine number (assigned
                     --   from 'fluidEngine').
  -> IRateSignal     -- ^ @channel@ - The channel number to use for the preset
                     --   in the given FluidSynth engine.
  -> IRateSignal     -- ^ @soundFontNumber@ - The number of the SoundFont
                     --   returned from 'fluidLoad'.
  -> IRateSignal     -- ^ @bank@ - The SoundFont bank number.
  -> IRateSignal     -- ^ @preset@ - The SoundFont preset number.
  -> StatementOpcode -- ^ The returned statement opcode.
fluidProgramSelect engine channel soundFontNumber bank preset =
  StatementOpcode "fluidProgramSelect"
    $ map getSignal [engine, channel, soundFontNumber, bank, preset]

-- | Loads an entire SoundFont2 (SF2) sample file into memory.
--
--   <https://csound.com/docs/manual/sfload.html Csound documentation>
sfload
  :: SRateSignal -- ^ @soundFont@ - The path to the SF2 file.
  -> IRateSignal -- ^ The SF2 identifier.
sfload soundFont = makeOpcodeSignal "sfload" [getSignal soundFont]

-- | Plays a SoundFont2 (SF2) sample preset, generating a stereo sound,
--   similarly to 'sfplay'. Unlike that opcode, though, it ignores the looping
--   points set in the SF2 file and substitutes them for a user-defined
--   crossfade loop.
--
--   This opcode only supports the sample structure of SF2 files. The
--   modulator structure of the SoundFont2 format is not supported in
--   Csound. Any modulation or processing to the sample data is left to the
--   Csound user, bypassing all restrictions forced by the SF2 standard.
--
--   Note: The looping points are set on the root key of every sample that
--   is part of the preset of the SoundFont. For instance, a SoundFont can
--   have a single sample for the whole keyboard. In that case, 'sflooper'
--   will work like 'flooper' and 'flooper2', because as the sample is
--   transposed/played back at different rates, the loop will get shorter or
--   longer. If, however, the SoundFont has a sample for each key, then there
--   will be no transposition and the loop will stay the same length
--   (unless the @transpositionRatio@ is changed).
--
--   <https://csound.com/docs/manual/sflooper.html Csound documentation>
sflooper
  :: IRateSignal   -- ^ @velocity@ - The MIDI velocity.
  -> IRateSignal   -- ^ @noteNumber@ - The MIDI note number.
  -> KRateSignal   -- ^ @amplitude@ - The amplitude scaling factor.
  -> KRateSignal   -- ^ @transpositionRatio@ - The transposition ratio.
                   --   Negative values are not allowed.
  -> IRateSignal   -- ^ @preset@ - The preset index.
  -> KRateSignal   -- ^ @loopStart@ - The loop start point (in seconds). Note
                   --   that although k-rate, loop parameters such as this
                   --   are only updated once per loop cycle. If the loop start
                   --   is set beyond the end of the sample, no looping will
                   --   result.
  -> KRateSignal   -- ^ @loopEnd@ - The loop end point (in seconds), updated
                   --   once per loop cycle.
  -> KRateSignal   -- ^ @crossfade@ - The crossfade length (in seconds),
                   --   updated once per loop cycle and limited to the loop
                   --   length.
  -> IRateSignal   -- ^ @playbackStart@ - The playback start position
                   --   (in seconds).
  -> IRateSignal   -- ^ @loopMode@ - The loop mode. It can be one of the
                   --   following:
                   --
                   --   * 0 (forward)
                   --   * 0 (backward)
                   --   * 2 (back and forth)
  -> IRateSignal   -- ^ @ftn@ - The crossfade envelope shape table number. If
                   --   zero, the crossfade is linear.
  -> IRateSignal   -- ^ @skipInit@ - If 1, the opcode initialisation is
                   --   skipped. For tied notes, performance continues from the
                   --   position in the loop where the previous note stopped.
  -> [ARateSignal] -- ^ The returned signals.
sflooper velocity noteNumber amplitude transpositionRatio preset loopStart loopEnd crossfade playbackStart loopMode ftn skipInit
  = makeOpcodeSignals "sflooper" args 2 where
  args =
    [ getSignal velocity
    , getSignal noteNumber
    , getSignal amplitude
    , getSignal transpositionRatio
    , getSignal preset
    , getSignal loopStart
    , getSignal loopEnd
    , getSignal crossfade
    , getSignal playbackStart
    , getSignal loopMode
    , getSignal ftn
    , getSignal skipInit
    ]

-- | 'sflooperWithDefaults' is identical to 'sflooper' with default values
--   supplied for @playbackStart@ (@'i#' 0@), @loopMode@ (@'i#' 0@),
--   @ftn@ (@'i#' 0@), and @skipInit@ (@'i#' 0@).
--
--   <https://csound.com/docs/manual/sflooper.html Csound documentation>
sflooperWithDefaults
  :: IRateSignal   -- ^ @velocity@ - The MIDI velocity.
  -> IRateSignal   -- ^ @noteNumber@ - The MIDI note number.
  -> KRateSignal   -- ^ @amplitude@ - The amplitude scaling factor.
  -> KRateSignal   -- ^ @transpositionRatio@ - The transposition ratio.
                   --   Negative values are not allowed.
  -> IRateSignal   -- ^ @preset@ - The preset index.
  -> KRateSignal   -- ^ @loopStart@ - The loop start point (in seconds). Note
                   --   that although k-rate, loop parameters such as this
                   --   are only updated once per loop cycle. If the loop start
                   --   is set beyond the end of the sample, no looping will
                   --   result.
  -> KRateSignal   -- ^ @loopEnd@ - The loop end point (in seconds), updated
                   --   once per loop cycle.
  -> KRateSignal   -- ^ @crossfade@ - The crossfade length (in seconds),
                   --   updated once per loop cycle and limited to the loop
                   --   length.
  -> [ARateSignal] -- ^ The returned signals.
sflooperWithDefaults velocity noteNumber amplitude transpositionRatio preset loopStart loopEnd crossfade
  = sflooper velocity
             noteNumber
             amplitude
             transpositionRatio
             preset
             loopStart
             loopEnd
             crossfade
             (i# 0)
             (i# 0)
             (i# 0)
             (i# 0)

-- | Assigns an existing preset of a previously loaded SoundFont2 (SF2)
--   sample file to an identifier.
--
--   <https://csound.com/docs/manual/sfpreset.html Csound documentation>
sfpreset
  :: IRateSignal -- ^ @program@ - The program number of a bank of presets in an
                 --   SF2 file.
  -> IRateSignal -- ^ @bank@ - The number of a specific bank of an SF2 file.
  -> IRateSignal -- ^ @identifier@ - The unique number generated by 'sfload' to
                 --   be used as an identifier for an SF2 file.
  -> IRateSignal -- ^ @preset@ - A unique preset identifier, to be used within
                 --   'sfplay'.
  -> IRateSignal -- ^ The preset identifier.
sfpreset program bank identifier preset = makeOpcodeSignal "sfpreset"
  $ map getSignal [program, bank, identifier, preset]
