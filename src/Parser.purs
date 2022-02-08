module Parser where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Class.Console (log)

test :: Effect Unit
test = do
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoChars "ABC"
  log $ show $ parse' threeChars "ABC"
  log $ show $ parse' threeChars "A"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"

class ParserError (e :: Type) where
  eof :: e

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
  apply p1 p2 = Parser \s -> case parse p1 s of
    Left err -> Left err
    Right (Tuple s1 h) -> case parse p2 s1 of
      Left err -> Left err
      Right (Tuple s2 x) -> Right $ Tuple s2 (h x)

instance applicativeParser :: Applicative (Parser e) where
  pure x = Parser \s -> Right $ Tuple s x

data PError = EOF
derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF

char :: forall e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing -> Left eof
  Just {head, tail} -> Right $ Tuple tail head

twoChars :: forall e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char

threeChars :: forall e. Parser e String
threeChars = (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

count :: forall e a f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count n  p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p)