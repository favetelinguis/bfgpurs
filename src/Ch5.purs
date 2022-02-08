module Ch5 where
  
  
import Prelude hiding (flip, const, apply, ($), (#))

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  log $ show $ null Nil
  log $ show $ null $ singleton 4
  log $ show $ snoc (1 : 2 : Nil) 3 
  log $ show $ length $ 1 : 2 : 3 : Nil
  log $ show (head Nil :: Maybe Unit)
  log $ show $ head ("abc" : "123" : Nil) 
  log $ show $ tail (Nil :: List Unit)
  log $ show $ tail ("abc" : "123" : Nil)
  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (1 : 2 : 3 : Nil) (-1)
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log $ show $ reverse (10 : 20 : 30 : Nil) 
  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log $ show $ range 1 10
  log $ show $ range 3 (-3)
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log <<< show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
  log $ show $ drop 2 (1:2:3:4:5:6:7:Nil)
  log $ show $ drop 10 (Nil::List Unit)
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)
  log$show$takeEnd 3 (1:2:3:4:5:6:Nil)
  log$show$takeEnd 10 (1:Nil)


flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x 

const :: forall a b. a -> b -> a
const x _ = x

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 0 applyFlipped as #

singleton :: forall a. a -> List a
singleton x = x : Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc Nil x = singleton x
snoc (x : xs) y = x : snoc xs y

length :: forall a. List a -> Int
length list = go 0 list
  where
    go acc Nil = acc
    go acc (_ : xs) = go (acc + 1) xs

head :: forall a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: forall a. List a ->  Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: forall a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: forall a. List a -> Maybe (List a)
init Nil = Nothing
init xs = Just $ go xs
  where
    go Nil = Nil
    go (_ : Nil) = Nil
    go (y : ys) = y : go ys

uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just {head: x, tail: xs}

{- Why is this not working?
bla :: List Int -> List Int -> List Int
bla xs ys = ado
   x <- xs 
   y <- ys
   in (const x y)
-}

index :: forall a. List a -> Int -> Maybe a
index Nil _ = Nothing
index _ i | i < 0 = Nothing
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i - 1)


infixl 8 index as !!

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex p l = go l 0
  where
    go Nil _ = Nothing
    go (x : _) agg | p x = Just agg
    go (_ : ys) agg = go ys (agg + 1)

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex p l = go l 0 Nothing
  where
    go :: List a -> Int -> Maybe Int -> Maybe Int
    go Nil _ i = i
    go (y : ys) agg _ | p y = go ys (agg + 1) (Just agg)
    go (_ : ys) agg i = go ys (agg + 1) i

reverse :: List ~> List
reverse Nil = Nil
reverse l = go Nil l
  where
    go agg Nil = agg
    go agg (x : xs) = go (x : agg) xs 

concat :: forall a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter pred (x : xs) =
  if pred x then x : filter pred xs
  else filter pred xs


catMaybes :: forall a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (x : xs) = 
  case x of
    Just y -> y : catMaybes xs
    Nothing -> catMaybes xs

range :: Int -> Int -> List Int
range start stop | start < stop = start : range (start + 1) stop
range start stop | start > stop = start : range (start - 1) stop
range _ stop = stop : Nil

take :: forall a. Int -> List a -> List a
take _ Nil = Nil
take 0 _ = Nil
take i (x : xs) = x : take (i - 1) xs

drop :: forall a. Int -> List a -> List a
drop _ Nil = Nil
drop 0 l = l
drop i (_ : xs) = drop (i - 1) xs

takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile pred (x : xs) = if pred x then x : takeWhile pred xs else Nil

dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l@(x : xs) = if pred x then dropWhile pred xs else l

takeEnd :: forall a. Int -> List a -> List a
takeEnd _ Nil = Nil
takeEnd i l@(_:xs) = if length l <= i then l else takeEnd i xs