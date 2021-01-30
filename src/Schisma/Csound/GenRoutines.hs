module Schisma.Csound.GenRoutines
  ( gen02
  , gen05
  , gen07
  , gen08
  ) where

import           Schisma.Utilities              ( interleave )

import           Schisma.Csound.Opcodes.FunctionTables
                                                ( ftgenonce )

import           Schisma.Csound.SignalGenerators
                                                ( i# )
import           Schisma.Csound.Types.Signals   ( IRateSignal )


-- | Transfers data into a function table.
--
--   <https://csound.com/docs/manual/GEN02.html Csound documentation>
gen02
  :: Bool          -- ^ @scale@ - Should the values be rescaled to an absolute
                   --   value of 1 after generation?
  -> [IRateSignal] -- ^ @tableArgs@ - The function table arguments.
  -> IRateSignal   -- ^ The returned signal, which contains the assigned table
                   --   number.
gen02 scale = ftgenonce (i# 0) routine
  where routine = if scale then i# 2 else i# (-2)

-- | Constructs functions from segments of exponential curves.
--
--   Discrete-point exponential interpolation implies that the progression
--   is by equal ratio.  The interpolation from one value to another is
--   such as to assume that the latter value will be attained in the
--   @length + 1@th location. For discontinuous functions, and for the
--   segment encompassing the end location, this value will not actually be
--   reached, although it may eventually appear as a result of final
--   scaling.
--
--   <https://csound.com/docs/manual/GEN05.html Csound documentation>
gen05
  :: Bool          -- ^ @scale@ - Should the values be rescaled to an absolute
                   --   value of 1 after generation?
  -> IRateSignal   -- ^ @size@ - The number of points in the table.
  -> [IRateSignal] -- ^ @values@ - The ordinate values. These must be nonzero
                   --   and must be alike in sign.
  -> [IRateSignal] -- ^ @lengths@ - The segment lengths. Lengths cannot be
                   --   negative, but a zero is meaningful for specifying
                   --   discontinuous waveforms. The sum of @lengths@ will
                   --   normally equal @size@ for fully specified
                   --   functions. If the sum is smaller, the function
                   --   locations not included will be set to zero; if the
                   --   sum is greater, only the first @size@ locations will
                   --   be stored. Note that the values are rounded to
                   --   integers before use.
  -> IRateSignal   -- ^ The returned signal, which contains the assigned table
                   --   number.
gen05 scale size values lengths = ftgenonce size routine args where
  routine = if scale then i# 5 else i# (-5)
  args    = interleave values lengths

-- | Constructs functions from segments of straight lines.
--
--   Discrete-point linear interpolation implies an increase or decrease
--   along a segment by equal differences between adjacent locations.
--   The interpolation from one value to another is such as to assume
--   that the latter value will be attained in the @length + 1@th location. For
--   discontinuous functions, and for the segment encompassing the end
--   location, this value will not actually be reached, although it may
--   eventually appear as a result of final scaling.
--
--   <https://csound.com/docs/manual/GEN07.html Csound documentation>
gen07
  :: Bool          -- ^ @scale@ - Should the values be rescaled to an absolute
                   --   value of 1 after generation?
  -> IRateSignal   -- ^ @size@ - The number of points in the table.
  -> [IRateSignal] -- ^ @values@ - The ordinate values.
  -> [IRateSignal] -- ^ @lengths@ - The segment lengths. Lengths cannot be
                   --   negative, but a zero is meaningful for specifying
                   --   discontinuous waveforms. The sum of @lengths@ will
                   --   normally equal @size@ for fully specified
                   --   functions. If the sum is smaller, the function
                   --   locations not included will be set to zero; if the
                   --   sum is greater, only the first @size@ locations will
                   --   be stored.
  -> IRateSignal   -- ^ The returned signal, which contains the assigned table
                   --   number.
gen07 scale size values lengths = ftgenonce size routine args where
  routine = if scale then i# 7 else i# (-7)
  args    = interleave values lengths

-- | Generate a piecewise cubic spline curve.
--
--   Each segment runs between two specified points but depends as well on
--   their neighbors on each side. Neighboring segments will agree in both
--   value and slope at their common point. (The common slope is that of
--   a parabola through that point and its two neighbors). The slope at the
--   two ends of the function is constrained to be zero (flat).
--
--   To make a discontinuity in slope or value in the function as stored,
--   arrange a series of points in the interval between two stored values;
--   likewise for a non-zero boundary slope.
--
--   <https://csound.com/docs/manual/GEN08.html Csound documentation>
gen08
  :: Bool          -- ^ @scale@ - Should the values be rescaled to an absolute
                   --   value of 1 after generation?
  -> IRateSignal   -- ^ @size@ - The number of points in the table.
  -> [IRateSignal] -- ^ @values@ - The ordinate values.
  -> [IRateSignal] -- ^ @lengths@ - The segment lengths. May not be zero, but
                   --   may be fractional. A particular segment may or may
                   --   not actually store any values; stored values will
                   --   be generated at integral points from the beginning
                   --   of the function. The sum of @lengths@ will normally
                   --   equal @size@ for fully specified functions.
  -> IRateSignal   -- ^ The returned signal, which contains the assigned table
                   --   number.
gen08 scale size values lengths = ftgenonce size routine args where
  routine = if scale then i# 8 else i# (-8)
  args    = interleave values lengths
