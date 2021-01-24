module Integration.Csound.Helpers
  (
  -- TODO: Fix
  -- , instrumentBlockCompare
  -- , linesToInstrumentBlock
  -- , signalsToInstrumentBlock
  -- , toText
  )
where

import           Test.Tasty.HUnit

import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Schisma.Csound.Opcodes.SignalOutput
import           Schisma.Csound.Renderer
import           Schisma.Csound.Types


-- TODO: Fix
-- instrumentBlockCompare :: Doc Text -> Doc Text -> Assertion
-- instrumentBlockCompare actual expected = toText actual @?= toText expected
--
-- linesToInstrumentBlock :: [Doc Text] -> Doc Text
-- linesToInstrumentBlock = concatWith (surround hardline)
--
-- signalsToInstrumentBlock :: [ARateSignal] -> Doc Text
-- signalsToInstrumentBlock signals = toInstrumentBlock instrument
--  where
--   opcode     = out signals
--   instrument = Instrument opcode 1
--
-- toText :: Doc Text -> Text
-- toText doc = renderStrict (layoutPretty defaultLayoutOptions doc)
