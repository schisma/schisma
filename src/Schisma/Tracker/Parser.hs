module Schisma.Tracker.Parser
  ( parseTrackerFile
  , parseTracker
  )
where

import           Control.Monad                  ( void )

import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( (\\)
                                                , elemIndex
                                                , transpose
                                                , zip3
                                                )

import qualified Data.Map.Strict                ( empty
                                                , fromList
                                                )

import           Data.Text                      ( Text
                                                , append
                                                , empty
                                                , isPrefixOf
                                                , lines
                                                , pack
                                                , split
                                                , strip
                                                )
import           Data.Text.IO                   ( readFile )

import           Data.Void                      ( Void )

import           Text.Megaparsec                ( Parsec
                                                , (<|>)
                                                , choice
                                                , errorBundlePretty
                                                , many
                                                , optional
                                                , parse
                                                , sepBy
                                                , some
                                                , try
                                                )
import           Text.Megaparsec.Char           ( alphaNumChar
                                                , char
                                                , digitChar
                                                , letterChar
                                                , printChar
                                                , spaceChar
                                                , string
                                                , upperChar
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )

import           Schisma.Tracker.Types


type Parser = Parsec Void Text

-- | Parses the contents within the supplied @file@ as a Tracker.
parseTrackerFile
  :: FilePath   -- ^ @file@ - The file.
  -> IO Tracker -- ^ The parsed Tracker.
parseTrackerFile file = do
  contents <- Data.Text.IO.readFile file
  return (parseTracker contents)

-- | Parses the supplied text @contents@ as a Tracker.
parseTracker
  :: Text    -- ^ @contents@ - The contents.
  -> Tracker -- ^ The parsed Tracker.
parseTracker contents = Tracker lineNumberTrack masterTrack instrumentTracks where
  allRows = Data.Text.lines contents
  removeJunk line      = not ("-" `isPrefixOf` line)
  rows                 = Prelude.filter removeJunk allRows

  headerColumns        = lineToColumns $ Prelude.head rows
  header               = parseHeader headerColumns

  masterTrackIndex     = findTrackIndex MasterHeader header
  lineNumberTrackIndex = findTrackIndex LineNumberHeader header

  allTrackIndices      = [0 .. (Prelude.length header - 1)]
  instrumentTrackIndices =
    allTrackIndices \\ [masterTrackIndex, lineNumberTrackIndex]

  bodyColumns = case rows of
    []     -> []
    x : xs -> Prelude.map lineToColumns xs
  tracks          = Data.List.transpose bodyColumns

  masterTrack     = parseMasterTrack $ tracks !! masterTrackIndex
  lineNumberTrack = parseLineNumberTrack $ tracks !! lineNumberTrackIndex

  toInstrumentTrack index = InstrumentTrack number mute solo cells   where
    InstrumentHeader number mute solo = header !! index
    cells = parseInstrumentTrack $ tracks !! index

  instrumentTracks = Prelude.map toInstrumentTrack instrumentTrackIndices

findTrackIndex :: HeaderCell -> [HeaderCell] -> Int
findTrackIndex key cells =
  fromMaybe (error $ "No " ++ show key ++ " column found") (elemIndex key cells)

lineToColumns :: Text -> [Text]
lineToColumns = split (== ',')

parseCell :: Parser a -> Text -> a
parseCell cellParser cell = case parse cellParser "" (strip cell) of
  Left  bundle  -> error (errorBundlePretty bundle)
  Right results -> results

parseMasterTrack :: [Text] -> [MasterCell]
parseMasterTrack = Prelude.map (parseCell parseMasterCell)

parseLineNumberTrack :: [Text] -> [Integer]
parseLineNumberTrack = Prelude.map (parseCell integerLiteral)

parseInstrumentTrack :: [Text] -> [NoteCell]
parseInstrumentTrack = Prelude.map (parseCell parseNoteCell)

parseHeader :: [Text] -> [HeaderCell]
parseHeader = Prelude.map (parseCell parseHeaderCell)

integerLiteral :: Parser Integer
integerLiteral = decimal

doubleLiteral :: Parser Double
doubleLiteral = read <$> (negativeParser <|> decimalParser <|> integerParser) where
  decimalParser :: Parser String
  decimalParser = try $ do
    front <- some digitChar
    void $ char '.'
    back <- some digitChar
    return $ front ++ ('.' : back)

  integerParser :: Parser String
  integerParser = try $ some digitChar

  negativeParser :: Parser String
  negativeParser = do
    void $ char '-'
    num <- decimalParser <|> integerParser
    return $ '-' : num

parseInstrumentEffect :: Parser (Text, Double)
parseInstrumentEffect = do
  name <- some letterChar
  void $ char ':'
  value <- doubleLiteral
  return (pack name, value)

parsePitch :: Parser Text
parsePitch = do
  name       <- some upperChar
  accidental <- many $ choice [char 'b', char '#']
  octave     <- some digitChar
  return $ pack (name ++ accidental ++ octave)

parseNoteOn :: Parser NoteCell
parseNoteOn = do
  pitch   <- parsePitch
  effects <- optional $ do
    void spaceChar
    sepBy parseInstrumentEffect spaceChar

  case effects of
    Nothing -> return (NoteOn pitch Data.Map.Strict.empty)
    Just fx -> return (NoteOn pitch $ Data.Map.Strict.fromList fx)

parseNoteOff :: Parser NoteCell
parseNoteOff = do
  void $ string "OFF"
  return NoteOff

parseNoteBlank :: Parser NoteCell
parseNoteBlank = do
  effects <- optional $ sepBy parseInstrumentEffect spaceChar

  case effects of
    Nothing -> return (NoteBlank Data.Map.Strict.empty)
    Just fx -> return (NoteBlank $ Data.Map.Strict.fromList fx)

parseNoteCell :: Parser NoteCell
parseNoteCell = parseNoteOff <|> parseNoteOn <|> parseNoteBlank

parseBeatsPerMinute :: Parser MasterSetting
parseBeatsPerMinute = do
  void $ string "bpm:"
  BeatsPerMinute <$> doubleLiteral

parseLinesPerBeat :: Parser MasterSetting
parseLinesPerBeat = do
  void $ string "lpb:"
  LinesPerBeat <$> doubleLiteral

parseTuning :: Parser MasterSetting
parseTuning = do
  void $ choice [string "tn:", string "tuning:"]
  tuning <- some alphaNumChar
  return (Tuning (pack tuning))

parseTemperament :: Parser MasterSetting
parseTemperament = do
  void $ choice [string "tm:", string "temperament:"]
  temperament <- some alphaNumChar
  return (Temperament (pack temperament))

parseMasterSetting :: Parser MasterSetting
parseMasterSetting =
  parseBeatsPerMinute <|> parseLinesPerBeat <|> parseTuning <|> parseTemperament

parseMasterCell :: Parser MasterCell
parseMasterCell = do
  settings <- sepBy parseMasterSetting spaceChar
  if Prelude.null settings
    then return MasterBlankSettings
    else return (MasterSettings settings)

parseLineNumberHeader :: Parser HeaderCell
parseLineNumberHeader = do
  void $ char '#'
  return LineNumberHeader

parseMasterHeader :: Parser HeaderCell
parseMasterHeader = do
  void $ string "Master"
  return MasterHeader

parseInstrumentHeader :: Parser HeaderCell
parseInstrumentHeader = do
  muteOrSolo <- optional $ choice [char 'M', char 'S']
  void $ char 'I'
  number <- integerLiteral
  name   <- optional $ do
    void spaceChar
    some printChar

  let (mute, solo) = case muteOrSolo of
        Nothing  -> (False, False)
        Just 'M' -> (True, False)
        Just 'S' -> (False, True)

  return (InstrumentHeader number mute solo)

parseHeaderCell :: Parser HeaderCell
parseHeaderCell =
  try parseInstrumentHeader <|> parseMasterHeader <|> parseLineNumberHeader
