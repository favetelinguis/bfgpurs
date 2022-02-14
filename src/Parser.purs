module Parser where

import Prelude

import Data.NonEmpty (NonEmpty, (:|), fromNonEmpty)
import Control.Lazy (class Lazy, defer)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray, uncons, singleton)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Class.Console (log)
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Data.String.CodePoints (codePointFromChar)
import Control.Alt (class Alt, (<|>))
import Data.Array ((:))

test :: Effect Unit
test = do
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoChars "ABC"
  log $ show $ parse' threeChars "ABC"
  log $ show $ parse' threeChars "A"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
  log $ show $ parse' (count' 3 digit) "123456"
  log $ show $ parse' (count' 3 digit) "abc456"
  log $ show $ parse' (count' 4 letter) "Freddy"
  log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5"
  log $ show $ parse' (count' 10 alphaNum) "######"
  log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3"
  log $ show $ parse' (atMost' 2 alphaNum) "$_$"
  log $ show $ parse' (atMost' 2 alphaNum) "a1b2c3"
  log $ show $ parse' yearFirst "1999-12-31"
  log $ show $ parse' monthFirst "12/31/1999"
  log $ show $ parse' (some' digit) "2343423$23abc"
  log $ show $ parse' (many' digit) "_2343423$23abc"
  log $ show $ parse' (some' digit) "_2343423$23abc"
  log $ show $ parse' ugly "17, some words"
  log $ show $ parse' ugly "5432, some more words1234567890"

class ParserError (e :: Type) where
  eof :: e
  invalidChar :: String -> e

type ParserState a = Tuple String a
type ParseFunction e a = 
  ParserError e => String -> Either e (ParserState a)
newtype Parser e a = Parser (ParseFunction e a)

parse :: forall e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

parse' :: forall a. Parser PError a -> ParseFunction PError a
parse' = parse

instance functorParser :: Functor (Parser e) where
  map f p = Parser \s -> map f <$> parse p s

instance applyParser :: Apply (Parser e) where
  apply = ap
                     
instance applicativeParser :: Applicative (Parser e) where
  pure x = Parser \s -> Right $ Tuple s x

data PError = EOF | InvalidChar String
derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF 
  invalidChar = InvalidChar

char :: forall e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing -> Left eof
  Just {head, tail} -> Right $ Tuple tail head

twoCharsA :: forall e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

twoCharsB :: forall e. Parser e (Tuple Char Char)
twoCharsB = char >>= \c1 -> char >>= \c2 -> pure $ Tuple c1 c2

twoChars :: forall e. Parser e (Tuple Char Char)
twoChars = do
        c1 <- char
        c2 <- char
        pure $ Tuple c1 c2

threeCharsA :: forall e. Parser e String
threeCharsA = (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

threeChars :: forall e. Parser e String
threeChars = do
        c1 <- char
        c2 <- char
        c3 <- char
        pure $ fromCharArray [c1, c2, c3]

count :: forall e a f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count n  p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p)

instance bindParser :: Bind (Parser e) where
        bind p f = Parser \s -> do
           Tuple s1 x <- parse p s
           parse (f x) s1

instance monadParser :: Monad (Parser e)

satisfy :: forall e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected pred = 
        char >>= \c -> if pred c then pure c else fail $ invalidChar expected

fail :: forall e a. ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

digit :: forall e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: forall e. ParserError e => Parser e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

instance altParser :: Alt (Parser e) where
        alt p1 p2 = Parser \s -> case parse p1 s of
                           Left _ -> parse p2 s
                           Right x -> Right x

alphaNum :: forall e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

count' :: forall e. Int -> Parser e Char -> Parser e String
count' n p = fromCharArray <$> count n p

newtype Year = Year Int
derive instance genericYear :: Generic Year _
instance showYear :: Show Year where
    show = genericShow

newtype Month = Month Int
derive instance genericMonth :: Generic Month _
instance showMonth :: Show Month where
    show = genericShow

newtype Day = Day Int
derive instance genericDay :: Generic Day _
instance showDay :: Show Day where
    show = genericShow
         
data DateFormat
        = YearFirst
        | MonthFirst
derive instance genericDateFormat :: Generic DateFormat _
instance showDateFormat :: Show DateFormat where
    show = genericShow

type DateParts =
        { year :: Year
        , month :: Month
        , day :: Day
        , format :: DateFormat
        }

atMost :: forall e a f
        . Unfoldable f
       => (a -> f a -> f a) ->  Int -> Parser e a -> Parser e (f a)
atMost cons n p
        | n <= 0 = pure none
        | otherwise = optional none $ p >>= \c -> cons c <$> atMost cons (n - 1) p

optional :: forall e a. a -> Parser e a -> Parser e a
optional x p = p <|> pure x

atMost' :: forall e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost (:) n p

range :: forall e a f
        . Semigroup (f a)
       => Traversable f
       => Unfoldable f 
       => (a -> f a -> f a) -> Int -> Int -> Parser e a -> Parser e (f a)
range cons min max p
        | min < 0
                || max <= 0
                || max < min = pure none
        | otherwise          = count min p >>= \cs -> (cs <> _) <$> atMost cons (max - min) p

range' :: forall e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range (:) min max p

yearFirst :: forall e. ParserError e => Parser e DateParts
yearFirst = do
        year <- Year <<< digitsToNum <$> count' 4 digit
        constChar '-'
        month <- Month <<< digitsToNum <$> range' 1 2 digit
        constChar '-'
        day <- Day <<< digitsToNum <$> range' 1 2 digit
        pure {year, month, day, format: YearFirst}

digitsToNum :: String -> Int
digitsToNum = fromMaybe 0 <<< fromString

constChar :: forall e. ParserError e => Char -> Parser e Unit
constChar = void <<< constChar'

monthFirst :: forall e. ParserError e => Parser e DateParts
monthFirst = do
        month <- Month <<< digitsToNum <$> range' 1 2 digit
        constChar '/'
        day <- Day <<< digitsToNum <$> range' 1 2 digit
        constChar '/'
        year <- Year <<< digitsToNum <$> count' 4 digit
        pure {year, month, day, format: MonthFirst}

date :: forall e. ParserError e => Parser e DateParts
date = yearFirst <|> monthFirst

some :: forall a f m
        . Unfoldable f
       => Alt m
       => Applicative m
       => Lazy (m (f a))
       => (a -> f a -> f a) -> m a -> m (NonEmpty f a)
some cons p = (:|) <$> p <*> defer \_ -> many cons p

many :: forall a f m 
        . Unfoldable f
       => Alt m
       => Applicative m
       => Lazy (m (f a))
       => (a -> f a -> f a) -> m a -> m (f a)
many cons p = fromNonEmpty cons <$> some cons p <|> pure none

instance lazyParser :: Lazy (Parser e a) where
        defer f = Parser \s -> parse (f unit) s 

some' :: forall e. Parser e Char -> Parser e String
some' p = fromCharArray <<< fromNonEmpty (:) <$> some (:) p

many' :: forall e. Parser e Char -> Parser e String
many' p = fromCharArray <$> many (:) p
        
digits :: forall e. ParserError e => Parser e String
digits = some' digit

ugly :: forall e. ParserError e => Parser e (Array String)
ugly = do
        p1 <- range' 1 4 digit
        constChar ','
        constChar ' '
        p2 <- some' (letter <|> constChar' ' ')
        p3 <- many' digit
        pure [p1, p2, p3]


constChar' :: forall e. ParserError e => Char -> Parser e Char
constChar' c = satisfy (singleton c) (_ == c)
