module Schisma.Csound.Opcodes.FunctionTables
  ( ftgenonce
  , ftgentmp
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types.Signals   ( IRateSignal
                                                , IsSignal(getSignal)
                                                )


-- | Enables the creation of function tables entirely inside instrument
--   definitions, without any duplication of data.
--
--   This opcode is designed to simplify writing instrument
--   definitions that can be re-used in different orchestras simply by
--   #including them and plugging them into some output instrument. There
--   is no need to define function tables either in the score, or in the
--   orchestra header.
--
--   This opcode is similar to 'ftgentmp', and has identical arguments.
--   However, function tables are neither duplicated nor deleted. Instead,
--   all of the arguments to the opcode are concatenated to form the key to
--   a lookup table that points to the function table number. Thus, every
--   request to 'ftgenonce' with the same arguments receives the same
--   instance of the function table data. Every change in the value of any
--   'ftgenonce' argument causes the creation of a new function table.
--
--   <https://csound.com/docs/manual/ftgenonce.html Csound documentation>
ftgenonce
  :: IRateSignal   -- ^ @size@ - The table size. Note that Csound was
                   --   originally designed to support tables with power of two
                   --   sizes only. Though this has changed in recent versions
                   --   (you can use any size by using a negative number), many
                   --   opcodes will not accept them.
  -> IRateSignal   -- ^ @genRoutine@ - The function table GEN routine.
  -> [IRateSignal] -- ^ @tableArgs@ - The function table arguments.
  -> IRateSignal   -- ^ The returned signal, which contains the assigned table
                   --   number.
ftgenonce size genRoutine tableArgs = makeOpcodeSignal "ftgenonce" args
  where args = map getSignal $ [i# 0, i# 0, size, genRoutine] ++ tableArgs

-- | Generate a score function table from within the orchestra, which is
--   optionally deleted at the end of the note.
--
--   <https://csound.com/docs/manual/ftgentmp.html Csound documentation>
ftgentmp
  :: IRateSignal   -- ^ @size@ - The table size. Note that Csound was
                   --   originally designed to support tables with power of two
                   --   sizes only. Though this has changed in recent versions
                   --   (you can use any size by using a negative number), many
                   --   opcodes will not accept them.
  -> IRateSignal   -- ^ @genRoutine@ - The function table GEN routine.
  -> [IRateSignal] -- ^ @tableArgs@ - The function table arguments.
  -> IRateSignal   -- ^ The returned signal, which contains the assigned table
                   --   number.
ftgentmp size genRoutine tableArgs = makeOpcodeSignal "ftgentmp" args
  where args = map getSignal $ [i# 0, i# 0, size, genRoutine] ++ tableArgs
