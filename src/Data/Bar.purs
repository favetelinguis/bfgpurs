module Bar where

import Data.Functor (class Functor)
import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Prelude (show)

-- Bid Ask
-- AskBar BidBar BidOpen BidHigh BidLow BidClose AskOpen AskHigh AskLow AskClose
-- Bar Open High Low Close Price
{-
newtype Price = Price Number

newtype Bid = Bid Price
newtype Ask = Ask Price
newtype AskBar = AskBar Bar -- functor so i can apply all Bar functions on an AskBar?
newtype BidBar = BidBar Bar

newtype Open  = Open Price
newtype High  = High Price
newtype Low  = Low Price
newtype Close  = Close Price

data Bar = Bar Open High Low Close

derive newtype instance showPrice :: Show Price
derive newtype instance showBid :: Show Bid
derive newtype instance showAsk :: Show Ask
derive newtype instance showOpen :: Show Open
derive newtype instance showHigh :: Show High
derive newtype instance showLow :: Show Low
derive newtype instance showClose :: Show Close

derive instance genericBar :: Generic Bar _
instance showBar :: Show Bar where
  show = genericShow
-}
--newtype Price = Price Number
--data OHLC = Open Price | High Price | Low Price | Close Price
-- Alt 1
--data Price = Ask Number | Bid Number
{-- Alt 2
data Sentiment a = Ask a | Bid a
instance functorSentiment :: Functor Sentiment where
  map :: forall a b. (a -> b) -> Sentiment a -> Sentiment b
  map f (Ask a) = Ask (f a)
  map f (Bid a) = Bid (f a)
bar :: Sentiment Bar
price :: Sentiment Price
I can now map over sentiment to use functions from bar and price but still be in a specific sentiment context
-}
type BarRow r = 
  (open :: Number, high:: Number, low :: Number, close :: Number | r)

-- Example composing row types
type DecisionRow r = 
  (a :: Int, b :: String | r)
newtype WorldState = WorldState { | BarRow (DecisionRow ())}

--data Bar = AskBar BarContent | BidBar BarContent

exampleDestructureBar :: { | BarRow ()} -> Int
exampleDestructureBar { open, high, low, close } = 4

{-
getHigh :: AskBar -> Ask
getHigh (AskBar (Bar _ (High h) _ _)) = Ask h

getHigh :: BidBar -> Bid
getHigh (BidBar (Bar _ (High h) _ _)) = Bid h
--data Price = Bid Number | Ask Number

-- All Prices should be the same type for a bar
--data Bar = Bara {o :: Price, h:: Price, l :: Price, c :: Price, time :: Int}

type Series = Array Bar

data OrderDirection = Sell | Buy
data OrderType = Limit | Market | OTC

--sell :: Ask -> 
--sell a = 

--buy :: Bid -> Bid
--buy p = sell p
--buy :: Bid -> String
-}