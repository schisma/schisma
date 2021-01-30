module Schisma.Csound.Renderer
  ( defaultOrchestraHeaderStatements
  , renderCsd
  , toInstrumentBlock
  ) where

import           Data.List                      ( foldl'
                                                , map
                                                , sortBy
                                                , unwords
                                                )
import           Data.Map.Strict                ( Map
                                                , assocs
                                                , elems
                                                , fromList
                                                )
import qualified Data.Map.Strict               as Map

import           Data.Containers.ListUtils      ( nubOrd
                                                , nubOrdOn
                                                )
import           Data.Set                       ( Set
                                                , (\\)
                                                , empty
                                                , map
                                                , toList
                                                , union
                                                )


import           Data.Text                      ( Text
                                                , append
                                                , intercalate
                                                , pack
                                                , strip
                                                , unpack
                                                )
import           Data.Text.Prettyprint.Doc      ( Doc
                                                , concatWith
                                                , hardline
                                                , pretty
                                                , surround
                                                )
import           Data.Text.Prettyprint.Doc.Render.Text
                                                ( hPutDoc )

import           System.FilePath                ( (</>) )
import           System.IO                      ( IOMode(WriteMode)
                                                , withFile
                                                )

import           Schisma.Csound.Orchestra       ( opcodeToInstrumentState
                                                , ratePrefix
                                                )

import           Schisma.Csound.Types.Compilation
                                                ( InstrumentLine
                                                  ( instrumentExpressionBody
                                                  , instrumentExpressionId
                                                  , instrumentExpressionOutputIds
                                                  )
                                                , InstrumentState
                                                  ( customUdos
                                                  , includedOpcodes
                                                  , instrumentLines
                                                  )
                                                )
import           Schisma.Csound.Types.Csound    ( Csd
                                                  ( csdInstruments
                                                  , csdOptions
                                                  , csdOrchestraHeaderStatements
                                                  , csdScore
                                                  )
                                                )
import           Schisma.Csound.Types.Instruments
                                                ( Instrument(Instrument) )
import           Schisma.Csound.Types.Score     ( FunctionTableStatement
                                                  ( functionTableActionTime
                                                  , functionTableGenRoutine
                                                  , functionTableGenRoutineParameters
                                                  , functionTableNumber
                                                  , functionTableSize
                                                  )
                                                , InstrumentStatement
                                                  ( instrumentDurationTime
                                                  , instrumentNumber
                                                  , instrumentParameters
                                                  , instrumentStartingTime
                                                  )
                                                , ScoreStatement(..)
                                                )
import           Schisma.Csound.Types.Signals   ( Udo
                                                  ( udoControlPeriodSamples
                                                  , udoInputRates
                                                  , udoName
                                                  , udoOpcode
                                                  , udoOutputRates
                                                  )
                                                )

import           ProjectPaths                   ( getIncludesDir )


-- | Contains the default orchestra header statements. Values are derived
--   from Csound defaults as well as common usage.
--
--   <https://csound.com/docs/manual/OrchTop.html Csound documentation>
defaultOrchestraHeaderStatements :: Map Text Text -- ^ The map containing the header statements.
defaultOrchestraHeaderStatements = fromList
  [ ("sr"     , "sr = 44100")
  , ("ksmps"  , "ksmps = 100")
  , ("nchnls" , "nchnls = 2")
  , ("0dbfs"  , "0dbfs = 1")
  , ("massign", "massign 0, 0")
  ]

-- | Compiles the supplied @csd@ to valid Csound code and renders it to
--   @filename@.
renderCsd
  :: Csd      -- ^ @csd@ - The Csd to be compiled.
  -> FilePath -- ^ @filename@ - The file location.
  -> IO ()    -- ^
renderCsd csd filename = do
  let options     = csdOptions csd
  let header      = elems $ csdOrchestraHeaderStatements csd

  let instruments = nubOrdOn (\(Instrument _ x) -> x) $ csdInstruments csd

  let instrumentOpcode (Instrument opcode _) = opcode
  let instrumentNumber (Instrument _ number) = number
  let instrumentStates =
        Data.List.map (opcodeToInstrumentState . instrumentOpcode) instruments
  let numbers          = Data.List.map instrumentNumber instruments
  let lines            = Data.List.map instrumentLines instrumentStates

  let instrumentBlocks = concat $ zipWith toInstrumentBlock numbers lines

  let ((udos, udoBlocks), includes) =
        foldl' toUdoBlocksAndIncludes ((empty, []), empty) instrumentStates

  includesDir <- getIncludesDir
  let includesFiles         = Data.Set.map (append includesDir) includes
  let includeStatementLines = Data.Set.map toIncludeStatementLine includesFiles

  let scoreStatementLines   = Data.List.map toScoreStatementLine (csdScore csd)

  let doc = toCsd options
                  header
                  (toList includeStatementLines)
                  udoBlocks
                  instrumentBlocks
                  scoreStatementLines

  withFile filename WriteMode (`hPutDoc` doc)

toCsd :: Text -> [Text] -> [Text] -> [Text] -> [Text] -> [Text] -> Doc Text
toCsd options orchestraHeaderStatements includeStatementLines udoBlocks instrumentLines score
  = let
      headerStatements     = Data.List.map pretty orchestraHeaderStatements
      includeStatements    = Data.List.map pretty includeStatementLines
      instrumentBlockLines = Data.List.map pretty instrumentLines
      udoBlockLines        = Data.List.map pretty udoBlocks
      scoreStatements =
        concatWith (surround hardline) (Data.List.map pretty score)
      lines =
        [ "<CsoundSynthesizer>"
          , "<CsOptions>"
          , pretty options
          , "</CsOptions>"
          , "<CsInstruments>"
          ]
          ++ headerStatements
          ++ includeStatements
          ++ udoBlockLines
          ++ instrumentBlockLines
          ++ [ "</CsInstruments>"
             , "<CsScore>"
             , scoreStatements
             , "</CsScore>"
             , "</CsoundSynthesizer>"
             ]
    in
      concatWith (surround hardline) lines

toFunctionTableStatementLine :: FunctionTableStatement -> Text
toFunctionTableStatementLine functionTableStatement = pack line
 where
  p1    = functionTableNumber functionTableStatement
  p2    = functionTableActionTime functionTableStatement
  p3    = functionTableSize functionTableStatement
  p4    = functionTableGenRoutine functionTableStatement
  pRest = functionTableGenRoutineParameters functionTableStatement
  params =
    Data.List.map show (Data.List.map fromInteger [p1, p2, p3, p4] ++ pRest)
  line = "f" ++ Data.List.unwords params

toIncludeStatementLine :: Text -> Text
toIncludeStatementLine filename =
  "#include \"" `append` filename `append` ".udo\""

-- | Converts the @number@ and @lines@ to a Csound instrument block.
toInstrumentBlock
  :: Integer          -- ^ @number@ - The instrument number.
  -> [InstrumentLine] -- ^ @lines@ - The instrument lines.
  -> [Text]           -- ^ The instrument block text.
toInstrumentBlock number lines = textLines where
  values = sortBy
    (\a b -> compare (instrumentExpressionId a) (instrumentExpressionId b))
    lines
  expressions = Data.List.map
    (\value ->
      strip
        $        intercalate ", " (instrumentExpressionOutputIds value)
        `append` instrumentExpressionBody value
    )
    values
  textLines =
    ["instr " `append` pack (show number)] ++ expressions ++ ["endin"]

toInstrumentStatementLine :: InstrumentStatement -> Text
toInstrumentStatementLine instrumentStatement = pack line
 where
  p1     = instrumentNumber instrumentStatement
  p2     = instrumentStartingTime instrumentStatement
  p3     = instrumentDurationTime instrumentStatement
  pRest  = instrumentParameters instrumentStatement
  params = Data.List.map show ([p1, p2, p3] ++ pRest)
  line   = "i" ++ Data.List.unwords params

toScoreStatementLine :: ScoreStatement -> Text
toScoreStatementLine statement = case statement of
  IStatement instrumentStatement ->
    toInstrumentStatementLine instrumentStatement
  FStatement functionTableStatement ->
    toFunctionTableStatementLine functionTableStatement

toUdoBlocksAndIncludes
  :: ((Set Udo, [Text]), Set Text)
  -> InstrumentState
  -> ((Set Udo, [Text]), Set Text)
toUdoBlocksAndIncludes ((priorUdos, priorUdoBlocks), priorIncludes) instrumentState
  = let udos          = customUdos instrumentState
        includes      = includedOpcodes instrumentState

        udosToProcess = udos \\ priorUdos

        blocks = toList $ Data.Set.map toUserDefinedOpcodeBlock udosToProcess
        udoStates     = Data.List.map fst blocks
        udoBlocks     = concatMap snd blocks

        newUdos       = priorUdos `union` udosToProcess
        newUdoBlocks  = udoBlocks ++ priorUdoBlocks
        newIncludes   = priorIncludes `union` includes
    in  foldl' toUdoBlocksAndIncludes
               ((newUdos, newUdoBlocks), newIncludes)
               udoStates

toUserDefinedOpcodeBlock :: Udo -> (InstrumentState, [Text])
toUserDefinedOpcodeBlock udo =
  let inputRates  = udoInputRates udo
      outputRates = udoOutputRates udo

      outputTypes = intercalate "" (Data.List.map ratePrefix outputRates)
      inputTypes  = if null inputRates
        then "0"
        else intercalate "" (Data.List.map ratePrefix inputRates)

      ksmps           = udoControlPeriodSamples udo

      instrumentState = opcodeToInstrumentState $ udoOpcode udo
      lines           = instrumentLines instrumentState

      values          = sortBy
        (\a b -> compare (instrumentExpressionId a) (instrumentExpressionId b))
        lines
      expressions = Data.List.map
        (\value ->
          strip
            $        intercalate ", " (instrumentExpressionOutputIds value)
            `append` instrumentExpressionBody value
        )
        values

      textLines =
        [ "opcode "
          `append` udoName udo
          `append` ", "
          `append` outputTypes
          `append` ", "
          `append` inputTypes
          ]
          ++ ["setksmps " `append` pack (show ksmps)]
          ++ expressions
          ++ ["endop"]
  in  (instrumentState, textLines)
