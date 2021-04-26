module Integration.Csound.Helpers
  ( signalsToInstrumentBlock
  ) where

import           Data.Text                      ( Text )

import           Schisma.Csound.Opcodes.SignalOutput
                                                ( out )
import           Schisma.Csound.Orchestra       ( opcodeToInstrumentState )
import           Schisma.Csound.Renderer        ( toInstrumentBlock )
import           Schisma.Csound.Types.Compilation
                                                ( InstrumentState
                                                  ( instrumentLines
                                                  )
                                                )
import           Schisma.Csound.Types.Instruments
                                                ( Instrument(Instrument) )

import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , Opcode(TerminalOpcode)
                                                , OrdinaryStatement(Op)
                                                )


signalsToInstrumentBlock :: [ARateSignal] -> [Text]
signalsToInstrumentBlock signals = toInstrumentBlock number lines
 where
  opcode          = TerminalOpcode $ Op $ out signals
  number          = 1
  instrument      = Instrument opcode number

  instrumentState = opcodeToInstrumentState opcode
  lines           = instrumentLines instrumentState
