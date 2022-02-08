module Ch15 where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:))
import Data.Profunctor (class Profunctor, dimap)
import Data.String (length)
import Effect (Effect)
import Effect.Class.Console (log)


test :: Effect Unit
test = do
  log $ show $ odd 0
  log $ show $ odd 1
  log "-----------------"
  log $ show $ runPredicate (Predicate odd) $ 10
  log $ show $ runPredicate (Predicate odd) $ 11
  log "-----------------"
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
  log "-----------------"
  log $ show $ runFoldL addr [1,2,3]
  log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
  log $ show $ runFoldL sizer ["This", "is", "the", "test"]

odd :: Int -> Boolean
odd x = x .&. 1 == 1

data Predicate a = Predicate (a -> Boolean)

runPredicate :: forall a. Predicate a -> a -> Boolean
runPredicate (Predicate f) x = f x

instance contravariantPredicate :: Contravariant Predicate where
  cmap f (Predicate g) = Predicate (g <<< f) 

-- s ConnectionState
data ConnectionState = NoSession | GotSession | SessionExpires | StreamConnected | StreamDisconnected

-- b Action
data ConnectionAction = NoOp | SetupState | SetupStream | RefreshState

-- outputFn :: ConnectionState -> ConnectionAction

data ConnectionChange = SessionCreated | SessionExpired | StreamDisconnected2 | StreamConnected2

-- transitionFn :: ConnectionState -> ConnectionChange -> ConnectionState
-- Probably need to separate Session control and stream control and also subscription control

-- state outputFn transitionFn
data Moore s a b = Moore s (s -> b) (s -> a -> s)
-- Moore s is Covariant in b Contravariant in a

instance profunctorMoore :: Profunctor (Moore s) where
  dimap f g (Moore s0 output transition) = Moore s0 (g <<< output) (\s -> transition s <<< f)

addr :: forall a. Semiring a => Moore a a a
addr = Moore zero identity (+)

runFoldL :: forall s a b f. Foldable f => Moore s a b -> f a -> b
runFoldL (Moore s0 output transition) = output <<< foldl transition s0

sizer :: Moore Int String String
sizer = dimap length (\s -> "Size is " <> show s) addr