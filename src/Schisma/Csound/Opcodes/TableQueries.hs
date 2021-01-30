module Schisma.Csound.Opcodes.TableQueries
  ( ftlen
  , nsamp
  ) where

import           Schisma.Csound.SignalGenerators
                                                ( makeOpcodeSignal )
import           Schisma.Csound.Types.Signals   ( IRateSignal
                                                , IsSignal(getSignal)
                                                )

-- | Returns the size (number of points, excluding guard point) of stored
--   function table, number @ftn@.  While most units referencing a stored
--   table will automatically take its size into account (so tables can be
--   of arbitrary length), this function reports the actual size if that is
--   needed. Note that 'ftlen' will always return a power-of-2 value, i.e.
--   the function table guard point is not included.
--
--   'ftlen' differs from 'nsamp' in that 'nsamp' gives the number of sample
--   frames loaded, while 'ftlen' gives the total number of samples without
--   the guard point. For example, with a stereo sound file of 10000
--   samples, 'ftlen' would return 19999 (i.e. a total of 20000 mono
--   samples, not including a guard point), but 'nsamp' returns 10000.
--
--  <https://csound.com/docs/manual/ftlen.html Csound documentation>
ftlen
  :: IRateSignal -- ^ @ftn@ - The function table number.
  -> IRateSignal -- ^ The returned signal.
ftlen ftn = makeOpcodeSignal "ftlen" [getSignal ftn]

-- | Returns the number of samples loaded into stored function table number
--   @ftn@. This is useful when a sample is shorter than the power-of-two
--   function table that holds it.
--
--   'nsamp' differs from 'ftlen' in that 'nsamp' gives the number of sample
--   frames loaded, while 'ftlen' gives the total number of samples. For
--   example, with a stereo sound file of 10000 samples, 'ftlen' would
--   return 19999 (i.e. a total of 20000 mono samples, not including
--   a guard point), but 'nsamp' returns 10000.
--
--  <https://csound.com/docs/manual/nsamp.html Csound documentation>
nsamp
  :: IRateSignal -- ^ @ftn@ - The function table number.
  -> IRateSignal -- ^ The returned signal.
nsamp ftn = makeOpcodeSignal "nsamp" [getSignal ftn]
