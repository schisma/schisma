module Schisma.Csound.Opcodes.TableReadWriteOperations
  ( tabmorphak
  )
where

import           Schisma.Csound.SignalGenerators
                                                ( makeOpcodeSignal )
import           Schisma.Csound.Types

-- | 'tabmorphak' allows morphing between a set of tables of the same size,
--   by means of a weighted average between two currently selected tables.
--
--   Firstly the user has to provide a set of tables of equal length
--   (@ftns@). Then they can choose a pair of tables in the set in order to
--   perform the morphing: @setIndex1@ and @setIndex2@ are filled with
--   numbers (zero represents the first table in the set, 1 the second,
--   2 the third and so on). Then determine the morphing between the two
--   chosen tables with the @weight@ parameter. After that the resulting
--   table can be indexed with the @index@ parameter like a normal table
--   opcode. If the value of this parameter surpasses the length of tables
--   (which must be the same for all tables), then it is wrapped around.
--
--   <https://csound.com/docs/manual/tabmorphak.html Csound documentation>
tabmorphak
  :: ARateSignal   -- ^ @index@ - The main index of the morphed resultant
                   --   table. The range is 0 to table_length (not included).
  -> KRateSignal   -- ^ @weight@ - The weight of the influence of a pair of
                   --   selected tables in the morphing. The range of this
                   --   argument is 0 to 1. A zero makes it output the
                   --   first table unaltered, a 1 makes it output the
                   --   second table of the pair unaltered. All
                   --   intermediate values between 0 and 1 determine the
                   --   gradual morphing between the two tables of the
                   --   pair.
  -> KRateSignal   -- ^ @setIndex1@ - The first table chosen for the morphing.
                   --   This number doesn’t express the table number
                   --   directly, but the index of the table in the set
                   --   (@ftns@), starting from 0 to N-1. If this number is
                   --   an integer, the corresponding table will be chosen
                   --   unaltered. If it contains fractional values, then
                   --   an interpolation with the next adjacent table will
                   --   result.
  -> KRateSignal   -- ^ @setIndex2@ - The second table chosen for the morphing.
                   --   This number doesn’t express the table number
                   --   directly, but the index of the table in the set
                   --   (@ftns@), starting from 0 to N-1. If this number is
                   --   an integer, the corresponding table will be chosen
                   --   unaltered. If it contains fractional values, then
                   --   an interpolation with the next adjacent table will
                   --   result.
  -> [IRateSignal] -- ^ @ftns@ - The function table numbers. This is a set of
                   --   chosen tables the user wants to use in the morphing.
                   --   All tables must have the same length. Be aware that
                   --   only two of these tables can be chosen for the
                   --   morphing at one time. Since it is possible to use
                   --   non-integer numbers for the @setIndex1@ and
                   --   @setIndex2@ arguments, the morphing is the result
                   --   from the interpolation between adjacent consecutive
                   --   tables of the set.
  -> ARateSignal   -- ^ The returned signal.
tabmorphak index weight setIndex1 setIndex2 ftns = makeOpcodeSignal
  "tabmorphak"
  args where
  args =
    [ getSignal index
      , getSignal weight
      , getSignal setIndex1
      , getSignal setIndex2
      ]
      ++ map getSignal ftns
