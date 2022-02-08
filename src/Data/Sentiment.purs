module Data.Sentiment where

import Data.Functor (class Functor)
import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)

data Sentiment a
  = Ask a
  | Bid a -- Posetiv (I will buy placing bid and get filled at best ask) | Negative (I will sell placing ask and getting filled a best bid) | Neutral
derive instance functorSentiment :: Functor Sentiment
{-
instance functorSentiment :: Functor Sentiment where
  map f (Ask a) = Ask (f a)
  map f (Bid a) = Bid (f a)
-}
derive instance genericSentiment :: Generic (Sentiment a) _

instance showSentiment :: Show a => Show (Sentiment a) where
  show c = genericShow c
