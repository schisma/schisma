module Schisma.Csound.Opcodes.TableAccess
  ( Table(..)
  )
where

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , i#
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types

class (IsSignal a, SignalGenerator b) => Table a b where
  -- | Accesses table values by direct indexing.
  --
  --   'table' invokes table lookup on behalf of its @index@.
  --
  --   'table' indexed by a periodic phasor will simulate an oscillator.
  --
  --   <https://csound.com/docs/manual/table.html Csound documentation>
  table
    :: a           -- ^ @index@ - The index. The index can be a raw entry number
                   --   (0,l,2...size - 1) or scaled values (0 to 1-e). Indices
                   --   are first modified by the @offset@ value then checked for
                   --   range before table lookup (see @wrap@). If index is
                   --   likely to be full scale, or if interpolation is being
                   --   used, the table should have an extended guard point.
    -> IRateSignal -- ^ @ftn@ - The function table number.
    -> IRateSignal -- ^ @mode@ - The index data mode. One may use any of the
                   --   following values for @mode@:
                   --
                   --   * 0: raw index
                   --   * 1: normalized (0 to 1)
    -> IRateSignal -- ^ @offset@ - The amount by which @index@ is to be offset.
                   --   For a table with origin at center, use @tablesize/2@
                   --   (raw) or 0.5 (normalized).
    -> IRateSignal -- ^ @wrap@ - The wraparound index flag. One may use any of
                   --   the following values for @wrap@:
                   --
                   --   * 0: nowrap (@index@ < 0 treated as @index@ = 0;
                   --        @index@ > tablesize sticks at @index@ = size)
                   --   * 1: wraparound
    -> b           -- ^ The returned signal.
  table index ftn mode offset wrap = makeOpcodeSignal "table" args where
    args =
      [ getSignal index
      , getSignal ftn
      , getSignal mode
      , getSignal offset
      , getSignal wrap
      ]

  -- | 'tableWithDefaults' is identical to 'table with default values supplied
  --   for @mode@ (@'i#' 0@), @offset@ (@'i#' 0@), and @wrap@ (@'i#' 0@).
  --
  --   <https://csound.com/docs/manual/table.html Csound documentation>
  tableWithDefaults
    :: a           -- ^ @index@ - The index. The index can be a raw entry number
                   --   (0,l,2...size - 1) or scaled values (0 to 1-e). Indices
                   --   are first modified by the @offset@ value then checked for
                   --   range before table lookup (see @wrap@). If index is
                   --   likely to be full scale, or if interpolation is being
                   --   used, the table should have an extended guard point.
    -> IRateSignal -- ^ @ftn@ - The function table number.
    -> b           -- ^ The returned signal.
  tableWithDefaults index ftn = table index ftn (i# 0) (i# 0) (i# 0)

instance Table ARateSignal ARateSignal
instance Table KRateSignal KRateSignal
instance Table IRateSignal IRateSignal
