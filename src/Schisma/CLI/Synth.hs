module Schisma.CLI.Synth
  ( synths
  ) where


import           Schisma.Synth.Parameters       ( profit
                                                , soundFont
                                                )

import           Schisma.Synth.Types            ( Synth(..) )

synths :: [Synth]
synths =
  [ Synth { synthName = "Profit", synthParameters = profit }
  , Synth { synthName = "SoundFont", synthParameters = soundFont }
  ]
