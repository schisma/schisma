module Schisma.Csound.Opcodes.Display
  ( print
  , printk
  , printkWithDefaults
  ) where

import           Prelude                 hiding ( print )

import           Schisma.Csound.SignalGenerators
                                                ( i# )
import           Schisma.Csound.Types.Signals   ( IRateSignal
                                                , IsSignal(getSignal)
                                                , KRateSignal
                                                , StatementOpcode(..)
                                                )

-- | Prints i-rate values.
--
--   The 'print' opcode will truncate decimal places and may not show the
--   complete value. Csound's precision depends on whether it is the floats
--   (32-bit) or double (64-bit) version, since most internal calculations
--   use one of these formats. If you need more resolution in the console
--   output, you can try 'printf'.
--
--   <https://csound.com/docs/manual/print.html Csound documentation>
print
  :: [IRateSignal]   -- ^ @signals@ - The signals to print.
  -> StatementOpcode -- ^ The returned opcode.
print signals = StatementOpcode "print" $ map getSignal signals

-- | Prints one k-rate value at specified intervals.
--
--   'printk' prints one k-rate value on every k-cycle, every second or at
--   intervals specified. First the instrument number is printed, then the
--   absolute time in seconds, then a specified number of spaces, then the
--   @signal@ value. The variable number of spaces enables different values to
--   be spaced out across the screen - so they are easier to view.
--
--   This opcode can be run on every k-cycle it is run in the instrument. To
--   accomplish this, set @interval@ to 0.
--
--   When @interval@ is not 0, the opcode prints on the first k-cycle it is
--   called, and subsequently when every @interval@ has elapsed. The time
--   cycles start from the time the opcode is initialized - typically the
--   initialization of the instrument.
--
--   <https://csound.com/docs/manual/printk.html Csound documentation>
printk
  :: IRateSignal     -- ^ @interval@ - Time in seconds between printings.
  -> KRateSignal     -- ^ @signal@ - The signal whose value should be printed.
  -> IRateSignal     -- ^ @spaces@ - The number of spaces to insert before
                     --   printing (maximum value is 130).
  -> IRateSignal     -- ^ @includeName@ - Should the name of @signal@ be
                     --   printed as well?
  -> StatementOpcode -- ^ The returned opcode.
printk interval signal spaces includeName = StatementOpcode
  "printk"
  [getSignal interval, getSignal signal, getSignal includeName]

-- | 'printkWithDefaults' is identical to 'printk' with default values supplied
--   for @spaces@ (@'i#' 0@) and @includeName@ (@'i#' 0@).
--
--   <https://csound.com/docs/manual/printk.html Csound documentation>
printkWithDefaults
  :: IRateSignal     -- ^ @interval@ - Time in seconds between printings.
  -> KRateSignal     -- ^ @signal@ - The signal whose value should be printed.
  -> StatementOpcode -- ^ The returned opcode.
printkWithDefaults interval signal = printk interval signal (i# 0) (i# 0)
