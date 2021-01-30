module Schisma.Csound.Orchestra
  ( opcodeToInstrumentState
  , ratePrefix
  ) where

import           Prelude                 hiding ( concat )

import           Data.Containers.ListUtils      ( nubOrd )
import           Data.List                      ( drop
                                                , foldl'
                                                , scanl'
                                                , take
                                                )
import           Data.Map.Strict                ( (!)
                                                , Map
                                                , elems
                                                , empty
                                                , member
                                                , singleton
                                                )
import           Data.Set                       ( Set
                                                , empty
                                                , singleton
                                                , union
                                                )
import           Data.Text                      ( Text
                                                , append
                                                , concat
                                                , drop
                                                , intercalate
                                                , pack
                                                , take
                                                )

import           Data.Text.Read                 ( decimal )


import           Schisma.Csound.Types.Compilation
                                                ( ConditionalBlock(..)
                                                , InstrumentLine
                                                  ( InstrumentLine
                                                  , instrumentExpressionId
                                                  , instrumentExpressionOutputIds
                                                  )
                                                , InstrumentState(..)
                                                )
import           Schisma.Csound.Types.Signals   ( Conditional(..)
                                                , Opcode(..)
                                                , OrdinaryStatement(..)
                                                , Signal
                                                  ( outputNumber
                                                  , outputOpcode
                                                  )
                                                , SignalRate(..)
                                                , StatementOpcode
                                                  ( StatementOpcode
                                                  )
                                                , Udo(udoName, udoOutputRates)
                                                )

import           Schisma.Utilities              ( listDifference
                                                , merge
                                                )

assignAliases :: InstrumentState -> Opcode -> InstrumentState
assignAliases instrumentState opcode =
  let bindings      = opcodeBindings instrumentState
      outputIds     = bindings ! opcode
      aliasedStates = scanl'
        (\acc outputId -> opcodeAssignment acc
                                           opcode
                                           [resultIdentifierRate outputId]
                                           (" = " `append` outputId)
        )
        instrumentState
        outputIds

      finalState = last aliasedStates
      finalLines = instrumentLines finalState
      aliasedIds = concatMap (\state -> opcodeBindings state ! opcode)
                             (tail aliasedStates)
      opcodeMap = Data.Map.Strict.singleton opcode aliasedIds
  in  updateInstrumentState finalState
                            finalLines
                            opcodeMap
                            Data.Set.empty
                            Data.Set.empty

conditionalBlock :: InstrumentState -> [Signal] -> ConditionalBlock
conditionalBlock instrumentState signals =
  let priorLines      = instrumentLines instrumentState

      outputOpcodes   = nubOrd $ map outputOpcode signals
      translatedState = foldl' translateOpcode instrumentState outputOpcodes
      aliasedState    = foldl' assignAliases translatedState outputOpcodes
      bodyLines       = listDifference (instrumentLines aliasedState) priorLines

      opcodeOutputIds =
        nubOrd $ concatMap (opcodeBindings aliasedState !) outputOpcodes

      conditionalOpcodes = includedOpcodes translatedState
      conditionalUdos    = customUdos translatedState
  in  ConditionalBlock { conditionalLines           = bodyLines
                       , conditionalOpcodeOutputIds = opcodeOutputIds
                       , conditionalIncludedOpcodes = conditionalOpcodes
                       , conditionalCustomUdos      = conditionalUdos
                       }

inputIdentifiers :: InstrumentState -> [Signal] -> [Text]
inputIdentifiers instrumentState signals = identifiers
 where
  bindings    = opcodeBindings instrumentState
  opcodes     = map outputOpcode signals
  numbers     = map outputNumber signals
  identifiers = zipWith (!!)
                        (map (bindings !) opcodes)
                        (map (subtract 1 . fromIntegral) numbers)

newLine :: InstrumentState -> [Text] -> Text -> [InstrumentLine]
newLine instrumentState outputIds body = priorLines ++ [line]
 where
  priorLines   = instrumentLines instrumentState
  expressionId = nextExpressionIdentifier priorLines
  line         = InstrumentLine outputIds body expressionId

nextExpressionIdentifier :: [InstrumentLine] -> Integer
nextExpressionIdentifier lines = maximum (0 : ids) + 1
  where ids = map instrumentExpressionId lines

nextResultIdentifier :: Map Opcode [Text] -> Integer
nextResultIdentifier opcodeMap = maximum (0 : ids) + 1
  where ids = map (resultIdentifier . last) $ elems opcodeMap

opcodeAssignment
  :: InstrumentState -> Opcode -> [SignalRate] -> Text -> InstrumentState
opcodeAssignment instrumentState opcode outputRates assignment =
  let outputIds  = outputIdentifiers instrumentState outputRates
      finalLines = newLine instrumentState outputIds assignment
      opcodeMap  = Data.Map.Strict.singleton opcode outputIds
  in  updateInstrumentState instrumentState
                            finalLines
                            opcodeMap
                            Data.Set.empty
                            Data.Set.empty

-- | Recursively translates an Opcode into an InstrumentState.
opcodeToInstrumentState
  :: Opcode          -- ^ @opcode@ - The opcode.
  -> InstrumentState -- ^ The translated InstrumentState.
opcodeToInstrumentState = translateOpcode instrumentState where
  instrumentState = InstrumentState { instrumentLines = []
                                    , opcodeBindings  = Data.Map.Strict.empty
                                    , includedOpcodes = Data.Set.empty
                                    , customUdos      = Data.Set.empty
                                    }

outputIdentifiers :: InstrumentState -> [SignalRate] -> [Text]
outputIdentifiers instrumentState outputRates = outputIds
 where
  opcodeMap            = opcodeBindings instrumentState
  baseOutputIdentifier = nextResultIdentifier opcodeMap
  outputCount          = fromIntegral $ length outputRates
  outputIds            = if outputCount == 0
    then []
    else zipWith
      (\rate id -> ratePrefix rate `append` pack (show id))
      outputRates
      [baseOutputIdentifier .. (baseOutputIdentifier + outputCount - 1)]

-- | Determines the Csound variable prefix from the @rate@.
ratePrefix
  :: SignalRate -- ^ @rate@ - The signal rate.
  -> Text       -- ^ The prefix.
ratePrefix ARate = "a"
ratePrefix IRate = "i"
ratePrefix KRate = "k"
ratePrefix SRate = "S"

reassignLines :: [InstrumentLine] -> [Text] -> [InstrumentLine]
reassignLines lines outputIds =
  let numberIntact = length lines - length outputIds
      reassign =
        zipWith
          (\outputId line -> line { instrumentExpressionOutputIds = [outputId] }
          )
      reassigned = reassign outputIds (Data.List.drop numberIntact lines)
  in  Data.List.take numberIntact lines ++ reassigned

resultIdentifier :: Text -> Integer
resultIdentifier = either error fst . decimal . Data.Text.drop 1

resultIdentifierRate :: Text -> SignalRate
resultIdentifierRate text = case Data.Text.take 1 text of
  "a" -> ARate
  "i" -> IRate
  "k" -> KRate
  "s" -> SRate

translateConditional
  :: InstrumentState -> Conditional -> (InstrumentState, Text)
translateConditional instrumentState conditional = case conditional of
  Predicate operator (leftOperand, rightOperand) ->
    let
      inputs          = [leftOperand, rightOperand]
      opcodes         = map outputOpcode inputs
      translatedState = foldl' translateOpcode instrumentState opcodes
      inputIds        = inputIdentifiers translatedState inputs

      clause =
        concat ["(", head inputIds, " ", operator, " ", last inputIds, ")"]
    in
      (translatedState, clause)
  CompoundPredicate operator (leftConditional, rightConditional) ->
    let (leftState, leftClause) =
          translateConditional instrumentState leftConditional
        (rightState, rightClause) =
          translateConditional leftState rightConditional

        clause = concat [leftClause, " ", operator, " ", rightClause]
    in  (rightState, clause)

translateOpcode :: InstrumentState -> Opcode -> InstrumentState
translateOpcode instrumentState opcode =
  if member opcode (opcodeBindings instrumentState)
    then instrumentState
    else case opcode of
      ConstDouble n outputRate -> opcodeAssignment
        instrumentState
        opcode
        [outputRate]
        (" = " `append` pack (show n))

      ConstInteger n outputRate -> opcodeAssignment
        instrumentState
        opcode
        [outputRate]
        (" = " `append` pack (show n))

      ConstText x outputRate -> opcodeAssignment
        instrumentState
        opcode
        [outputRate]
        (" strcpyk " `append` pack (show x))

      PField n outputRate -> opcodeAssignment instrumentState
                                              opcode
                                              [outputRate]
                                              (" = p" `append` pack (show n))

      BinaryOperator operator (leftOperand, rightOperand) outputRate ->
        let
          inputs          = [leftOperand, rightOperand]
          opcodes         = map outputOpcode inputs
          translatedState = foldl' translateOpcode instrumentState opcodes
          inputIds        = inputIdentifiers translatedState inputs
          assignment =
            concat [" = ", head inputIds, " ", operator, " ", last inputIds]
        in
          opcodeAssignment translatedState opcode [outputRate] assignment

      ConditionalExpression conditional (ifSignals, elseSignals) ->
        let
          (translatedState, clause) =
            translateConditional instrumentState conditional

          clauseExpression = concat ["if ", clause, " then"]
          linesWithClause  = newLine translatedState [] clauseExpression

          ifBlock          = conditionalBlock
            (translatedState { instrumentLines = linesWithClause })
            ifSignals
          ifBodyLines = conditionalLines ifBlock

          ifState =
            translatedState { instrumentLines = linesWithClause ++ ifBodyLines }
          linesWithElse = newLine ifState [] "else"


          elseBlock     = conditionalBlock
            (translatedState { instrumentLines = linesWithElse })
            elseSignals
          elseBodyLines = conditionalLines elseBlock

          elseState =
            translatedState { instrumentLines = linesWithElse ++ elseBodyLines }
          linesWithEndIf      = newLine elseState [] "endif"


          ifOpcodeOutputIds   = conditionalOpcodeOutputIds ifBlock
          elseOpcodeOutputIds = conditionalOpcodeOutputIds elseBlock

          maxIfResultId = maximum $ 0 : map resultIdentifier ifOpcodeOutputIds
          maxElseResultId =
            maximum $ 0 : map resultIdentifier elseOpcodeOutputIds

          finalResults = if maxIfResultId > maxElseResultId
            then ifOpcodeOutputIds
            else elseOpcodeOutputIds

          finalIfBodyLines   = reassignLines ifBodyLines finalResults
          finalElseBodyLines = reassignLines elseBodyLines finalResults

          finalLines =
            listDifference linesWithEndIf (ifBodyLines ++ elseBodyLines)
              ++ finalIfBodyLines
              ++ finalElseBodyLines

          opcodeMap          = Data.Map.Strict.singleton opcode finalResults

          conditionalOpcodes = conditionalIncludedOpcodes ifBlock
            `union` conditionalIncludedOpcodes elseBlock

          conditionalUdos = conditionalCustomUdos ifBlock
            `union` conditionalCustomUdos elseBlock
        in
          updateInstrumentState translatedState
                                finalLines
                                opcodeMap
                                conditionalOpcodes
                                conditionalUdos

      Opcode name inputs outputRates ->
        let opcodes         = map outputOpcode inputs
            translatedState = foldl' translateOpcode instrumentState opcodes

            outputIds       = outputIdentifiers translatedState outputRates
            inputIds        = inputIdentifiers translatedState inputs

            body = concat [" ", name, " ", Data.Text.intercalate ", " inputIds]
            finalLines      = newLine translatedState outputIds body

            opcodeMap       = Data.Map.Strict.singleton opcode outputIds
        in  updateInstrumentState translatedState
                                  finalLines
                                  opcodeMap
                                  Data.Set.empty
                                  Data.Set.empty

      IncludedOpcode name inputs outputRates ->
        let opcodes         = map outputOpcode inputs
            translatedState = foldl' translateOpcode instrumentState opcodes

            outputIds       = outputIdentifiers translatedState outputRates
            inputIds        = inputIdentifiers translatedState inputs

            body = concat [" ", name, " ", Data.Text.intercalate ", " inputIds]
            finalLines      = newLine translatedState outputIds body

            opcodeMap       = Data.Map.Strict.singleton opcode outputIds
        in  updateInstrumentState translatedState
                                  finalLines
                                  opcodeMap
                                  (Data.Set.singleton name)
                                  Data.Set.empty

      UserDefinedOpcode udo inputs ->
        let opcodes         = map outputOpcode inputs
            translatedState = foldl' translateOpcode instrumentState opcodes

            outputRates     = udoOutputRates udo
            outputIds       = outputIdentifiers translatedState outputRates
            inputIds        = inputIdentifiers translatedState inputs

            name            = udoName udo
            body = concat [" ", name, " ", Data.Text.intercalate ", " inputIds]
            finalLines      = newLine translatedState outputIds body

            opcodeMap       = Data.Map.Strict.singleton opcode outputIds
        in  updateInstrumentState translatedState
                                  finalLines
                                  opcodeMap
                                  Data.Set.empty
                                  (Data.Set.singleton udo)

      MutatingOpcode name inputs mutatedIndices ->
        let
          opcodes         = map outputOpcode inputs
          translatedState = foldl' translateOpcode instrumentState opcodes

          inputIds        = inputIdentifiers translatedState inputs

          expression = concat [name, " ", Data.Text.intercalate ", " inputIds]
          finalLines      = newLine translatedState [] expression

          mutatedIds      = map ((inputIds !!) . fromIntegral) mutatedIndices
          opcodeMap       = Data.Map.Strict.singleton opcode mutatedIds
        in
          updateInstrumentState translatedState
                                finalLines
                                opcodeMap
                                Data.Set.empty
                                Data.Set.empty

      PassthroughOpcode (StatementOpcode name inputs) passthroughSignal ->
        let
          opcodes         = map outputOpcode (passthroughSignal : inputs)
          translatedState = foldl' translateOpcode instrumentState opcodes

          inputIds        = inputIdentifiers translatedState inputs
          expression = concat [name, " ", Data.Text.intercalate ", " inputIds]
          finalLines      = newLine translatedState [] expression

          passthroughIds  = inputIdentifiers translatedState [passthroughSignal]
          opcodeMap       = Data.Map.Strict.singleton opcode passthroughIds
        in
          updateInstrumentState translatedState
                                finalLines
                                opcodeMap
                                Data.Set.empty
                                Data.Set.empty

      TerminalOpcode statement ->
        translateOrdinaryStatement instrumentState statement

translateOrdinaryStatement
  :: InstrumentState -> OrdinaryStatement -> InstrumentState
translateOrdinaryStatement instrumentState statement = case statement of
  Op (StatementOpcode name inputs) ->
    let opcodes         = map outputOpcode inputs
        translatedState = foldl' translateOpcode instrumentState opcodes

        inputIds        = inputIdentifiers translatedState inputs

        body = concat [" ", name, " ", Data.Text.intercalate ", " inputIds]
        finalLines      = newLine translatedState [] body
    in  updateInstrumentState translatedState
                              finalLines
                              Data.Map.Strict.empty
                              Data.Set.empty
                              Data.Set.empty

  NoOp ->
    let finalLines = newLine instrumentState [] "; no-op"
    in  updateInstrumentState instrumentState
                              finalLines
                              Data.Map.Strict.empty
                              Data.Set.empty
                              Data.Set.empty

  ConditionalStatement conditional (ifStatement, elseStatement) ->
    let
      (translatedState, clause) =
        translateConditional instrumentState conditional

      clauseExpression = concat ["if ", clause, " then"]
      linesWithClause  = newLine translatedState [] clauseExpression

      ifState          = translateOrdinaryStatement
        (translatedState { instrumentLines = linesWithClause })
        ifStatement
      linesWithElse = newLine ifState [] "else"

      elseState     = translateOrdinaryStatement
        (translatedState { instrumentLines = linesWithElse })
        elseStatement
      linesWithEndIf = newLine elseState [] "endif"

      conditionalOpcodes =
        includedOpcodes ifState `union` includedOpcodes elseState

      conditionalUdos = customUdos ifState `union` customUdos elseState
    in
      updateInstrumentState translatedState
                            linesWithEndIf
                            Data.Map.Strict.empty
                            conditionalOpcodes
                            conditionalUdos

updateInstrumentState
  :: InstrumentState
  -> [InstrumentLine]
  -> Map Opcode [Text]
  -> Set Text
  -> Set Udo
  -> InstrumentState
updateInstrumentState instrumentState instrumentLines newBindings newIncludes newUdos
  = let existingBindings = opcodeBindings instrumentState
        existingIncludes = includedOpcodes instrumentState
        existingUdos     = customUdos instrumentState
    in  instrumentState { instrumentLines = instrumentLines
                        , opcodeBindings  = merge existingBindings newBindings
                        , includedOpcodes = existingIncludes `union` newIncludes
                        , customUdos      = existingUdos `union` newUdos
                        }
