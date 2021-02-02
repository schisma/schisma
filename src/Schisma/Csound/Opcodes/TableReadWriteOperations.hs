module Schisma.Csound.Opcodes.TableReadWriteOperations
  ( TableW(..)
  , tabmorphak
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , IRateSignal
                                                , IsSignal(getSignal)
                                                , KRateSignal
                                                , StatementOpcode(..)
                                                )

class (IsSignal a) => TableW a where
  -- | Change the contents of existing function tables.
  --
  --   This opcode operates on existing function tables, changing their
  --   contents. 'tablew' is for writing at k- or at a-rates, with the table
  --   number being specified at init time. Using 'tablew' with i-rate signal
  --   and index values is allowed, but the specified data will always be
  --   written to the function table at k-rate, not during the
  --   initialization pass.
  --
  --   <https://csound.com/docs/manual/tablew.html Csound documentation>
  tablew
    :: a               -- ^ @signal@ - The value to be written into the table.
    -> a               -- ^ @index@ - The table index. When @indexMode@ is @0@,
                       --   this must be a positive number whose upper
                       --   bound cannot exceed (@table length - 1@). When
                       --   @indexMode@ is not @0@, this value must be
                       --   constrained to a @0@ to
                       --   @1@ range.
    -> IRateSignal     -- ^ @ftn@ - The function table number. Must be @>= 1@.
                       --   Floats are rounded down to an integer. If a table
                       --   number does not point to a valid table, or the
                       --   table has not yet been loaded, then an error will
                       --   result and the instrument will be de-activated.
    -> IRateSignal     -- ^ @indexMode@ - The index data mode.
                       --
                       --   * If @0@, @index@ and @offset@ ranges match the
                       --     length of the table.
                       --   * If not @0@, @index@ and @offset@ have a @0@ to
                       --     @1@ range.
    -> IRateSignal     -- ^ @offset@ - The amount by which @index@ is to be
                       --   offset.
                       --
                       --   * If @0@, indexing starts from the start of the
                       --     table.
                       --   * If not @0@, indexing starts from somewhere else
                       --     in the table. The value must be less than the
                       --     table length if @index@ is @0@, or less than @1@
                       --     if @index@ is not @0@.
    -> IRateSignal     -- ^ @wrapGuardpointMode@ - The wrap and guardpoint
                       --   mode. Can be one of the following values:
                       --
                       --   * 0: Limit mode
                       --   * 1: Wrap mode
                       --   * 2: Guardpoint mode
    -> StatementOpcode -- ^ The returned signal.
  tablew signal index ftn indexMode offset wrapGuardpointMode = StatementOpcode "tablew" args where
    args =
      [ getSignal signal
      , getSignal index
      , getSignal ftn
      , getSignal indexMode
      , getSignal offset
      , getSignal wrapGuardpointMode
      ]

  -- | 'tablewWithDefaults' is identical to 'tablew' with default values
  --   supplied for @indexMode@ (@'i#' 0@), @offset@ (@'i#' 0@), and
  --   @wrapGuardpointMode@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/tablew.html Csound documentation>
  tablewWithDefaults
    :: a               -- ^ @signal@ - The value to be written into the table.
    -> a               -- ^ @index@ - The table index. When @indexMode@ is @0@,
                       --   this must be a positive number whose upper
                       --   bound cannot exceed (@table length - 1@). When
                       --   @indexMode@ is not @0@, this value must be
                       --   constrained to a @0@ to
                       --   @1@ range.
    -> IRateSignal     -- ^ @ftn@ - The function table number. Must be @>= 1@.
                       --   Floats are rounded down to an integer. If a table
                       --   number does not point to a valid table, or the
                       --   table has not yet been loaded, then an error will
                       --   result and the instrument will be de-activated.
    -> StatementOpcode -- ^ The returned signal.
  tablewWithDefaults signal index ftn = tablew signal index ftn (i# 0) (i# 0) (i# 0)

instance TableW ARateSignal
instance TableW KRateSignal
instance TableW IRateSignal


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
