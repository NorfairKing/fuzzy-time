{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FuzzyTime.Parser
  ( fuzzyZonedTimeP
  , fuzzyTimeOfDayP
  , atHourP
  , atMinuteP
  , atExactP
  , hourSegmentP
  , minuteSegmentP
  , twoDigitsSegmentP
  , fuzzyDayP
  , fuzzyDayOfTheWeekP
  , Parser
  ) where

import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Tree
import Data.Validity
import Data.Void

import Control.Monad

import Text.Megaparsec
import Text.Megaparsec.Char as Char
import Text.Megaparsec.Char.Lexer as Lexer

import Data.FuzzyTime.Types

type Parser = Parsec Void Text

fuzzyZonedTimeP :: Parser FuzzyZonedTime
fuzzyZonedTimeP = pure ZonedNow

fuzzyTimeOfDayP :: Parser FuzzyTimeOfDay
fuzzyTimeOfDayP =
  choice'
    [ recTreeParser
        [ ("midnight", Midnight)
        , ("midday", Noon)
        , ("noon", Noon)
        , ("morning", Morning)
        , ("evening", Evening)
        ]
    , atExactP
    , atMinuteP
    , atHourP
    , diffP
    ]

atHourP :: Parser FuzzyTimeOfDay
atHourP =
  label "AtHour" $ do
    h <- hourSegmentP
    pure $ AtHour h

atMinuteP :: Parser FuzzyTimeOfDay
atMinuteP =
  label "AtMinute" $ do
    h <- hourSegmentP
    void $ optional $ char ':'
    m <- minuteSegmentP
    pure $ AtMinute h m

atExactP :: Parser FuzzyTimeOfDay
atExactP =
  label "AtExact" $ do
    h <- hourSegmentP
    void $ optional $ char ':'
    m <- minuteSegmentP
    void $ char ':'
    s <- fromInteger <$> decimal
    pure $ AtExact $ TimeOfDay h m s

diffP :: Parser FuzzyTimeOfDay
diffP =
  label "Diff" $ do
    n <- signed (pure ()) decimal
    mc <- optional $ choice' [char 'h', char 'm', char 's']
    f <-
      case mc of
        Nothing -> pure HoursDiff
        Just 'h' -> pure HoursDiff
        Just 'm' -> pure MinutesDiff
        Just 's' -> pure (\i -> SecondsDiff $ fromIntegral i)
        _ -> fail "should not happen."
    pure $ f n

hourSegmentP :: Parser Int
hourSegmentP =
  label "hour segment" $ do
    h <- twoDigitsSegmentP
    guard $ h >= 0 && h < 24
    pure h

minuteSegmentP :: Parser Int
minuteSegmentP =
  label "minute segment" $ do
    m <- twoDigitsSegmentP
    guard $ m >= 0 && m < 60
    pure m

twoDigitsSegmentP :: Parser Int
twoDigitsSegmentP =
  label "two digit segment" $ do
    d1 <- digit
    md2 <- optional digit
    pure $
      case md2 of
        Nothing -> d1
        Just d2 -> 10 * d1 + d2

digit :: Parser Int
digit = do
  let l = ['0' .. '9']
  c <- oneOf l <?> "digit"
  case elemIndex c l of
    Nothing -> fail "Shouldn't happen."
    Just d -> pure d

-- | Can handle:
--
-- - yesterday
-- - now
-- - today
-- - tomorrow
-- - "%Y-%m-%d"
--
-- and all non-ambiguous prefixes
fuzzyDayP :: Parser FuzzyDay
fuzzyDayP =
  choice'
    [ recTreeParser
        [("yesterday", Yesterday), ("now", Now), ("today", Today), ("tomorrow", Tomorrow)]
    , fmap ExactDay (some (digitChar <|> char '-') >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
    , dayInMonthP
    , dayOfTheMonthP
    , NextDayOfTheWeek <$> fuzzyDayOfTheWeekP
    , diffDaysP
    ]

dayOfTheMonthP :: Parser FuzzyDay
dayOfTheMonthP = do
  v <- OnlyDay <$> Lexer.lexeme Char.space Lexer.decimal
  guard $ isValid v
  pure v

dayInMonthP :: Parser FuzzyDay
dayInMonthP = do
  m <- Lexer.lexeme Char.space Lexer.decimal
  guard (m >= 1)
  guard (m <= 12)
  void $ string "-"
  d <- Lexer.lexeme Char.space Lexer.decimal
  let v = DayInMonth m d
  guard $ isValid v
  pure v

diffDaysP :: Parser FuzzyDay
diffDaysP =
  fmap DiffDays $ do
    d <- Lexer.lexeme Char.space (Lexer.signed Char.space Lexer.decimal)
    void $ optional $ char 'd'
    pure d

-- | Can handle:
--
-- - monday
-- - tuesday
-- - wednesday
-- - thursday
-- - friday
-- - saturday
-- - sunday
--
-- and all non-ambiguous prefixes
fuzzyDayOfTheWeekP :: Parser DayOfTheWeek
fuzzyDayOfTheWeekP =
  recTreeParser
    [ ("monday", Monday)
    , ("tuesday", Tuesday)
    , ("wednesday", Wednesday)
    , ("thursday", Thursday)
    , ("friday", Friday)
    , ("saturday", Saturday)
    , ("sunday", Sunday)
    ]

recTreeParser :: [(String, a)] -> Parser a
recTreeParser tups = do
  let pf = makeParseForest tups
  s <- some letterChar
  case lookupInParseForest s pf of
    Nothing ->
      fail $ "Could not parse any of these recursively unambiguously: " ++ show (map fst tups)
    Just f -> pure f

lookupInParseForest :: Eq c => [c] -> Forest (c, Maybe a) -> Maybe a
lookupInParseForest = gof
  where
    gof :: Eq c => [c] -> Forest (c, Maybe a) -> Maybe a
    gof cs = msum . map (got cs)
    got :: Eq c => [c] -> Tree (c, Maybe a) -> Maybe a
    got [] _ = Nothing
    got (c:cs) Node {..} =
      let (tc, tma) = rootLabel
       in if tc == c
            then case cs of
                   [] -> tma
                   _ -> gof cs subForest
            else Nothing

makeParseForest :: Eq c => [([c], a)] -> Forest (c, Maybe a)
makeParseForest = foldl insertf []
  where
    insertf :: Eq c => Forest (c, Maybe a) -> ([c], a) -> Forest (c, Maybe a)
    insertf for ([], _) = for
    insertf for (c:cs, a) =
      case find ((== c) . fst . rootLabel) for of
        Nothing ->
          let got [] = Nothing
              got (c_:cs_) = Just $ Node (c_, Just a) $ maybeToList $ got cs_
           in case got (c : cs) of
                Nothing -> for -- Should not happen, but is fine
                Just t -> t : for
        Just n ->
          flip map for $ \t ->
            let (tc, _) = rootLabel t
             in if tc == c
                  then n {rootLabel = (tc, Nothing), subForest = insertf (subForest n) (cs, a)}
                  else t

choice' :: [Parser a] -> Parser a
choice' [] = empty
choice' [x] = x
choice' (a:as) = try a <|> choice' as
