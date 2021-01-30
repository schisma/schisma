module Schisma.Csound.Opcodes.SoftwareBus
  ( ChannelGet(..)
  , ChannelSet(..)
  , chngetks
  , chnsetks
  , xin
  , xout
  ) where


import           Schisma.Utilities              ( mapWithIndex )

import           Schisma.Csound.SignalGenerators
                                                ( SignalGenerator
                                                , makeOpcodeSignal
                                                )
import           Schisma.Csound.Types.Signals   ( ARateSignal
                                                , IRateSignal
                                                , IsSignal(..)
                                                , KRateSignal
                                                , Opcode(Opcode)
                                                , SRateSignal
                                                , Signal(Signal)
                                                , SignalRate
                                                , StatementOpcode(..)
                                                )

class (SignalGenerator a) => ChannelGet a where
  -- | Reads data from a channel of the inward named software bus.
  --
  --   The 'chnget' opcode works both at i-time and perf-time. String variables
  --   are only updated if the channel has changed.
  --
  --  <https://csound.com/docs/manual/chnget.html Csound documentation>
  chnget
    :: SRateSignal -- ^ @name@ - A string that identifies a channel of the
                   --   named software bus to read.
    -> a           -- ^ The returned signal.
  chnget name = makeOpcodeSignal "chnget" [getSignal name]

instance ChannelGet ARateSignal
instance ChannelGet KRateSignal
instance ChannelGet IRateSignal
instance ChannelGet SRateSignal


class (IsSignal a) => ChannelSet a where
  -- | Write to a channel of the named software bus.
  --
  --   The 'chnset' opcode works both at i-time and perf-time. Channel contents
  --   are only updated if the string variable is modified.
  --
  --  <https://csound.com/docs/manual/chnset.html Csound documentation>
  chnset
    :: a               -- ^ @value@ - The value to write.
    -> SRateSignal     -- ^ @name@ - A string that indicates which named
                       --   channel of the software bus to write to.
    -> StatementOpcode -- ^ The returned opcode.
  chnset value name = StatementOpcode "chnset" [getSignal value, getSignal name]

instance ChannelSet ARateSignal
instance ChannelSet KRateSignal
instance ChannelSet IRateSignal
instance ChannelSet SRateSignal


-- | Reads data from a channel of the inward named software bus.
--
--   'chngetks' works only at perf-time. String variables are only updated if
--   the channel has changed.
--
--   <https://csound.com/docs/manual/chnget.html Csound documentation>
chngetks
  :: SRateSignal -- ^ @name@ - A string that identifies a channel of the
                 --   named software bus to read.
  -> SRateSignal -- ^ The returned signal.
chngetks name = makeOpcodeSignal "chngetks" [getSignal name]

-- | Write to a channel of the named software bus.
--
--   'chnsetks' works only at perf-time. Channel contents are only updated if
--   the string variable is modified.
--
--   <https://csound.com/docs/manual/chnset.html Csound documentation>
chnsetks
  :: SRateSignal     -- ^ @value@ - The value to write.
  -> SRateSignal     -- ^ @name@ - A string that indicates which named
                     --   channel of the software bus to write to.
  -> StatementOpcode -- ^ The returned opcode.
chnsetks value name =
  StatementOpcode "chnsetks" [getSignal value, getSignal name]

-- | Passes variables to a user-defined opcode block.
--
--   The 'xin' opcode copies variables to the opcode definition, allowing
--   communication with the calling instrument.
--
--   This opcode actually runs only at i-time. Performance time copying is done
--   by the user opcode call.
--
--   <https://csound.com/docs/manual/xin.html Csound documentation>
xin
  :: [SignalRate]           -- ^ @inputRates@ - The input rates.
  -> [(Signal, SignalRate)] -- ^ The returned signals, coupled with their
                            --   corresponding rates.
xin inputRates = mapWithIndex toSignal inputRates
 where
  opcode = Opcode "xin" [] inputRates
  toSignal rate index = (Signal opcode (index + 1), rate)

-- | Retrieves variables from a user-defined opcode block.
--
--   The 'xout' opcode copies variables from the opcode definition, allowing
--   communication with the calling instrument.
--
--   This opcode actually runs only at i-time. Performance time copying is done
--   by the user opcode call.
--
--   <https://csound.com/docs/manual/xout.html Csound documentation>
xout
  :: [Signal]          -- ^ @signals@ - The signals to be returned from the
                       --   user-defined opcode.
  -> StatementOpcode   -- ^ The returned statement opcode.
xout = StatementOpcode "xout"
