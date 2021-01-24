module Schisma.Synth.Utilities where

import           Schisma.Csound.Opcodes.Arithmetic
                                                ( (+#)
                                                , (-#)
                                                , (*#)
                                                , (/#)
                                                , (^#)
                                                )
import           Schisma.Csound.Opcodes.MathematicalFunctions
                                                ( log# )
import           Schisma.Csound.SignalGenerators
                                                ( i#
                                                , k#
                                                )
import           Schisma.Csound.Types

-- | Blends a list of signals by scaling them equivalently.
blend
  :: [ARateSignal] -- ^ @signals@ - The list of signals.
  -> ARateSignal   -- ^ The blended signal.
blend signals = foldl1 (+#) scaledSignals
 where
  factor        = i# $ length signals
  scaledSignals = map (/# factor) signals

-- | Detunes a frequency.
detune
  :: KRateSignal -- ^ @frequency@ - The frequency.
  -> KRateSignal -- ^ @amount@ - The amount of cents to detune.
  -> KRateSignal -- ^ The detuned frequency.
detune frequency amount = (k# 2 ^# (amount /# k# 1200)) *# frequency

-- | Calculates the number of 12-TET semitones between two frequencies.
frequencyIntervalToSemitones
  :: IRateSignal -- ^ @frequency1@ - The first frequency.
  -> IRateSignal -- ^ @frequency2@ - The second frequency.
  -> IRateSignal -- ^ The number of 12-TET semitones.
frequencyIntervalToSemitones frequency1 frequency2 = semitones
 where
  -- semitones = (12 / ln(2)) * ln(f2 / f1)
  log2       = log# (i# 2)
  firstTerm  = i# 12 /# log2
  secondTerm = log# (frequency2 /# frequency1)
  semitones  = firstTerm *# secondTerm

-- | Mixes a list of signals according to their gain levels.
mix
  :: [(ARateSignal, KRateSignal)] -- ^ @signals@ - A list of tuples containing
                                  --   a signal and its corresponding gain
                                  --   level.
  -> ARateSignal                  -- ^ The mixed signal.
mix signals = foldl1 (+#) (map (uncurry (*#)) signals)

-- | Linearly scales a value from one range to another.
scaleLinearly
  :: IRateSignal -- ^ @value@ - The value to scale.
  -> IRateSignal -- ^ @lowerBound@ - The lower bound of the original range.
  -> IRateSignal -- ^ @upperBound@ - The upper bound of the original range.
  -> IRateSignal -- ^ @scaledMin@ - The minimum value of the new range.
  -> IRateSignal -- ^ @scaledMax@ - The maximum value of the new range.
  -> IRateSignal -- ^ The scaled value.
scaleLinearly value lowerBound upperBound scaledMin scaledMax =
  (value -# lowerBound)
    *# (scaledMax -# scaledMin)
    /# (upperBound -# lowerBound)
    +# scaledMin
