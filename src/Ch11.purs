module Ch11 where

import Data.Foldable (class Foldable)
import Data.List (List(..), foldl, (:))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), (:|))
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, class Semiring, type (~>), Unit, discard, negate, otherwise, show, zero, ($), (+), (>))

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 :30 : Nil)
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)
  log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ sum (1 : 2 : 3 : Nil)
  log $ show $ sum (1.0 : 2.0 : 3.0 : Nil)
  log $ show $ sum [1,2,3]
  log $ show $ sum [1.0,2.0,3.0]

reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil

max :: forall a. Ord a => a -> a -> a
max x y | x > y = x
        | otherwise = y

findMax :: forall a. Ord a => List a -> Maybe a
findMax Nil = Nothing
findMax l@(first : _) = Just $ foldl max first l 

findMaxNE :: forall a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList (NonEmpty a l)) = foldl max a l

sum :: forall a b. Foldable b => Semiring a => b a -> a
sum = foldl (+) zero

data Tree a = Leaf a | Node (Tree a) (Tree a)