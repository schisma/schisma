module Schisma.Csound.Opcodes.Strings
  ( strcat
  ) where


import           Schisma.Csound.SignalGenerators
                                                ( makeOpcodeSignal )
import           Schisma.Csound.Types

-- | Concatenate two strings. 'strcat' runs at i-time only.
--
--   <https://csound.com/docs/manual/strcat.html Csound documentation>
strcat
  :: SRateSignal -- ^ @string1@ - The first string.
  -> SRateSignal -- ^ @string2@ - The second string.
  -> SRateSignal -- ^ The returned opcode.
strcat string1 string2 =
  makeOpcodeSignal "strcat" $ map getSignal [string1, string2]
