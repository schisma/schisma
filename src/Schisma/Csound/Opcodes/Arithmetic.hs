module Schisma.Csound.Opcodes.Arithmetic
  ( Arithmetic(..)
  , Exponentiation(..)
  )
where

import           Schisma.Csound.Types

class (IsSignal a, IsSignal b, IsSignal c) => Arithmetic a b c | a b -> c where
  -- | Adds two signals together.
  --
  --  <https://csound.com/docs/manual/adds.html Csound documentation>
  (+#)
    :: a -- ^ @x@ - The first signal.
    -> b -- ^ @y@ - The second signal.
    -> c -- ^ The returned signal.

  -- | Subtracts one signal from another.
  --
  --  <https://csound.com/docs/manual/subtracts.html Csound documentation>
  (-#)
    :: a -- ^ @x@ - The first signal.
    -> b -- ^ @y@ - The second signal.
    -> c -- ^ The returned signal.

  -- | Multiplies two signals together.
  --
  --  <https://csound.com/docs/manual/multiplies.html Csound documentation>
  (*#)
    :: a -- ^ @x@ - The first signal.
    -> b -- ^ @y@ - The second signal.
    -> c -- ^ The returned signal.

  -- | Divides a signal by another one.
  --
  --  <https://csound.com/docs/manual/divides.html Csound documentation>
  (/#)
    :: a -- ^ @x@ - The first signal (the dividend).
    -> b -- ^ @y@ - The second signal (the divisor).
    -> c -- ^ The returned signal (the quotient).

  -- | The modulus operator.
  --
  --   The operator % returns the value of @x@ reduced by @y@, so that the
  --   result, in absolute value, is less than the absolute value of @y@, by
  --   repeated subtraction. This is the same as modulus function in
  --   integers.
  --
  --  <https://csound.com/docs/manual/modulus.html Csound documentation>
  (%#)
    :: a -- ^ @x@ - The first signal.
    -> b -- ^ @y@ - The second signal.
    -> c -- ^ The returned signal.

instance Arithmetic ARateSignal ARateSignal ARateSignal where
  x +# y = ARateSignal (add ARate (getSignal x) (getSignal y))
  x -# y = ARateSignal (sub ARate (getSignal x) (getSignal y))
  x *# y = ARateSignal (multiply ARate (getSignal x) (getSignal y))
  x /# y = ARateSignal (divide ARate (getSignal x) (getSignal y))
  x %# y = ARateSignal (modulus ARate (getSignal x) (getSignal y))

instance Arithmetic ARateSignal KRateSignal ARateSignal where
  x +# y = ARateSignal (add ARate (getSignal x) (getSignal y))
  x -# y = ARateSignal (sub ARate (getSignal x) (getSignal y))
  x *# y = ARateSignal (multiply ARate (getSignal x) (getSignal y))
  x /# y = ARateSignal (divide ARate (getSignal x) (getSignal y))
  x %# y = ARateSignal (modulus ARate (getSignal x) (getSignal y))

instance Arithmetic KRateSignal ARateSignal ARateSignal where
  x +# y = ARateSignal (add ARate (getSignal x) (getSignal y))
  x -# y = ARateSignal (sub ARate (getSignal x) (getSignal y))
  x *# y = ARateSignal (multiply ARate (getSignal x) (getSignal y))
  x /# y = ARateSignal (divide ARate (getSignal x) (getSignal y))
  x %# y = ARateSignal (modulus ARate (getSignal x) (getSignal y))

instance Arithmetic ARateSignal IRateSignal ARateSignal where
  x +# y = ARateSignal (add ARate (getSignal x) (getSignal y))
  x -# y = ARateSignal (sub ARate (getSignal x) (getSignal y))
  x *# y = ARateSignal (multiply ARate (getSignal x) (getSignal y))
  x /# y = ARateSignal (divide ARate (getSignal x) (getSignal y))
  x %# y = ARateSignal (modulus ARate (getSignal x) (getSignal y))

instance Arithmetic IRateSignal ARateSignal ARateSignal where
  x +# y = ARateSignal (add ARate (getSignal x) (getSignal y))
  x -# y = ARateSignal (sub ARate (getSignal x) (getSignal y))
  x *# y = ARateSignal (multiply ARate (getSignal x) (getSignal y))
  x /# y = ARateSignal (divide ARate (getSignal x) (getSignal y))
  x %# y = ARateSignal (modulus ARate (getSignal x) (getSignal y))

instance Arithmetic KRateSignal KRateSignal KRateSignal where
  x +# y = KRateSignal (add KRate (getSignal x) (getSignal y))
  x -# y = KRateSignal (sub KRate (getSignal x) (getSignal y))
  x *# y = KRateSignal (multiply KRate (getSignal x) (getSignal y))
  x /# y = KRateSignal (divide KRate (getSignal x) (getSignal y))
  x %# y = KRateSignal (modulus KRate (getSignal x) (getSignal y))

instance Arithmetic KRateSignal IRateSignal KRateSignal where
  x +# y = KRateSignal (add KRate (getSignal x) (getSignal y))
  x -# y = KRateSignal (sub KRate (getSignal x) (getSignal y))
  x *# y = KRateSignal (multiply KRate (getSignal x) (getSignal y))
  x /# y = KRateSignal (divide KRate (getSignal x) (getSignal y))
  x %# y = KRateSignal (modulus KRate (getSignal x) (getSignal y))

instance Arithmetic IRateSignal KRateSignal KRateSignal where
  x +# y = KRateSignal (add KRate (getSignal x) (getSignal y))
  x -# y = KRateSignal (sub KRate (getSignal x) (getSignal y))
  x *# y = KRateSignal (multiply KRate (getSignal x) (getSignal y))
  x /# y = KRateSignal (divide KRate (getSignal x) (getSignal y))
  x %# y = KRateSignal (modulus KRate (getSignal x) (getSignal y))

instance Arithmetic IRateSignal IRateSignal IRateSignal where
  x +# y = IRateSignal (add IRate (getSignal x) (getSignal y))
  x -# y = IRateSignal (sub IRate (getSignal x) (getSignal y))
  x *# y = IRateSignal (multiply IRate (getSignal x) (getSignal y))
  x /# y = IRateSignal (divide IRate (getSignal x) (getSignal y))
  x %# y = IRateSignal (modulus IRate (getSignal x) (getSignal y))


class (IsSignal a, IsSignal b, IsSignal c) => Exponentiation a b c | a b -> c where
  -- | "Power of" operator.
  --
  --  The operator raises @x@ to the @y@ power. @y@ may not be audio-rate.
  --
  --  <https://csound.com/docs/manual/raises.html Csound documentation>
  (^#)
    :: a -- ^ @x@ - The first signal (the base).
    -> b -- ^ @y@ - The second signal (the exponent).
    -> c -- ^ The returned signal.

instance Exponentiation ARateSignal KRateSignal ARateSignal where
  x ^# y = ARateSignal (powerOf ARate (getSignal x) (getSignal y))

instance Exponentiation KRateSignal KRateSignal KRateSignal where
  x ^# y = KRateSignal (powerOf KRate (getSignal x) (getSignal y))

instance Exponentiation KRateSignal IRateSignal KRateSignal where
  x ^# y = KRateSignal (powerOf KRate (getSignal x) (getSignal y))

instance Exponentiation IRateSignal KRateSignal KRateSignal where
  x ^# y = KRateSignal (powerOf KRate (getSignal x) (getSignal y))

instance Exponentiation IRateSignal IRateSignal IRateSignal where
  x ^# y = IRateSignal (powerOf IRate (getSignal x) (getSignal y))


infixl 6 +#
infixl 6 -#
infixl 7 *#
infixl 7 /#
infixl 7 %#
infixr 8 ^#


add :: SignalRate -> Signal -> Signal -> Signal
add rate signal1 signal2 = Signal opcode 1
  where opcode = BinaryOperator "+" (signal1, signal2) rate

sub :: SignalRate -> Signal -> Signal -> Signal
sub rate signal1 signal2 = Signal opcode 1
  where opcode = BinaryOperator "-" (signal1, signal2) rate

multiply :: SignalRate -> Signal -> Signal -> Signal
multiply rate signal1 signal2 = Signal opcode 1
  where opcode = BinaryOperator "*" (signal1, signal2) rate

divide :: SignalRate -> Signal -> Signal -> Signal
divide rate signal1 signal2 = Signal opcode 1
  where opcode = BinaryOperator "/" (signal1, signal2) rate

modulus :: SignalRate -> Signal -> Signal -> Signal
modulus rate signal1 signal2 = Signal opcode 1
  where opcode = BinaryOperator "%" (signal1, signal2) rate

powerOf :: SignalRate -> Signal -> Signal -> Signal
powerOf rate signal1 signal2 = Signal opcode 1
  where opcode = BinaryOperator "^" (signal1, signal2) rate
