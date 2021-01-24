module Schisma.Csound.SignalGenerators
  ( MultipleSignalGenerator(..)
  , SignalGenerator(..)
  , a#
  , i#
  , k#
  , pi#
  , pk#
  , stringSignal
  )
where

import           Data.Text                      ( Text )

import           Schisma.Csound.Types

class MultipleSignalGenerator a where
  -- | Creates the output signals for an opcode.
  makeOpcodeSignals
    :: Text     -- ^ @name@ - The opcode name.
    -> [Signal] -- ^ @inputs@ - The opcode inputs.
    -> Integer  -- ^ @count@ - The number of opcode outputs.
    -> a        -- ^ The returned signals.

instance MultipleSignalGenerator [ARateSignal] where
  makeOpcodeSignals name inputs count = map toSignal [1 .. count]
   where
    opcode   = Opcode name inputs $ replicate (fromIntegral count) ARate
    toSignal = ARateSignal . Signal opcode

instance MultipleSignalGenerator [KRateSignal] where
  makeOpcodeSignals name inputs count = map toSignal [1 .. count]
   where
    opcode   = Opcode name inputs $ replicate (fromIntegral count) KRate
    toSignal = KRateSignal . Signal opcode

instance MultipleSignalGenerator [IRateSignal] where
  makeOpcodeSignals name inputs count = map toSignal [1 .. count]
   where
    opcode   = Opcode name inputs $ replicate (fromIntegral count) IRate
    toSignal = IRateSignal . Signal opcode

instance MultipleSignalGenerator [SRateSignal] where
  makeOpcodeSignals name inputs count = map toSignal [1 .. count]
   where
    opcode   = Opcode name inputs $ replicate (fromIntegral count) SRate
    toSignal = SRateSignal . Signal opcode


class SignalGenerator a where
  -- | Creates an output signal for an opcode.
  makeOpcodeSignal
    :: Text     -- ^ @name@ - The opcode name.
    -> [Signal] -- ^ @inputs@ - The opcode inputs.
    -> a        -- ^ The returned signal.

  -- | Converts a rate-less signal into a rated signal.
  makeRatedSignal
    :: (Signal, SignalRate) -- ^ The signal along with its corresponding rate.
    -> a                    -- ^ The returned signal.

instance SignalGenerator ARateSignal where
  makeOpcodeSignal name inputs = ARateSignal $ Signal opcode 1
    where opcode = Opcode name inputs [ARate]

  makeRatedSignal (signal, ARate) = ARateSignal signal

instance SignalGenerator KRateSignal where
  makeOpcodeSignal name inputs = KRateSignal $ Signal opcode 1
    where opcode = Opcode name inputs [KRate]

  makeRatedSignal (signal, KRate) = KRateSignal signal

instance SignalGenerator IRateSignal where
  makeOpcodeSignal name inputs = IRateSignal $ Signal opcode 1
    where opcode = Opcode name inputs [IRate]

  makeRatedSignal (signal, IRate) = IRateSignal signal

instance SignalGenerator SRateSignal where
  makeOpcodeSignal name inputs = SRateSignal $ Signal opcode 1
    where opcode = Opcode name inputs [SRate]

  makeRatedSignal (signal, SRate) = SRateSignal signal


-- | Creates an a-rate signal from a number.
a#
  :: Real n
  => n           -- ^ @number@ - The number.
  -> ARateSignal -- ^ The returned signal.
a# number = ARateSignal signal where
  opcode = ConstDouble (realToFrac number) ARate
  signal = Signal opcode 1

-- | Creates an i-rate signal from a number.
i#
  :: Real n
  => n           -- ^ @number@ - The number.
  -> IRateSignal -- ^ The returned signal.
i# number = IRateSignal signal where
  opcode = ConstDouble (realToFrac number) IRate
  signal = Signal opcode 1

-- | Creates an k-rate signal from a number.
k#
  :: Real n
  => n           -- ^ @number@ - The number.
  -> KRateSignal -- ^ The returned signal.
k# number = KRateSignal signal where
  opcode = ConstDouble (realToFrac number) KRate
  signal = Signal opcode 1

-- | References a p-field as an i-rate signal.
pi#
  :: Integer     -- ^ @field@ - The p-field.
  -> IRateSignal -- ^ The returned signal.
pi# field = IRateSignal signal where
  opcode = PField field IRate
  signal = Signal opcode 1

-- | References a p-field as a k-rate signal.
pk#
  :: Integer     -- ^ @field@ - The p-field.
  -> KRateSignal -- ^ The returned signal.
pk# field = KRateSignal signal where
  opcode = PField field KRate
  signal = Signal opcode 1

-- | Creates an s-rate signal from text.
stringSignal
  :: Text        -- ^ @text@ - The text.
  -> SRateSignal -- ^ The returned signal.
stringSignal text = SRateSignal signal where
  opcode = ConstText text SRate
  signal = Signal opcode 1
