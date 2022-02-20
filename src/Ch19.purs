module Ch19 where

import Data.Tuple (Tuple(..))
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

a = 1

test :: Effect Unit
test = do 
        log $ show $ Just (_ * 10) <*> Just 20
        log $ show $ Just (_ * 10) <*> pure 20
        log $ show $ Just 20 >>= pure <<< (_ * 10)
        log $ show do
           x <- Just 20
           let y = x * 10
           pure y
        log $ show $ Just 20 >>= const Nothing >>= \y -> Just $ y + 42
        log $ show do
           _ <- Just 20
           y <- Nothing
           pure $ y + 42

        log $ show $ Right (_ * 10) <*> (Right 20 :: Either Unit _)
        log $ show $ Right (_ * 10) <*> (pure 20 :: Either Unit _)
        log $ show $ (Right 20 :: Either Unit _) >>= pure <<< (_ * 10)
        log $ show do
           x <- Right 20 :: Either Unit _
           let y = x * 10
           pure y
        log $ show $ Right 20 >>= const (Left "error") >>= \y -> Right $ y + 42
        log $ show do
           _ <- Right 20
           y <- (Left "error")
           pure $ y + 42
        log $ show
                $ runRWS rwsTest {r: {debugModeOn: true}, w: mempty, s: 0}

data Maybe a = Nothing | Just a
derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
        show = genericShow

instance functorMaybe :: Functor Maybe where
        map _ Nothing = Nothing
        map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
        apply Nothing _ = Nothing
        apply (Just g) f = g <$> f

instance applicativeMaybe :: Applicative Maybe where
        pure = Just

instance bindMaybe :: Bind Maybe where
        bind Nothing _ = Nothing
        bind (Just x) f = f x

instance monadMaybe :: Monad Maybe

data Either a b = Left a | Right b
derive instance functorEither :: Functor (Either a)
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
        show = genericShow

instance applyEither :: Apply (Either a) where
        apply (Left a) _ = Left a
        apply (Right g) f = g <$> f

instance applicativeEither :: Applicative (Either a) where
        pure = Right

instance bindEither :: Bind (Either a) where
        bind (Left a) _ = Left a
        bind (Right x) f = f x

instance monadEither :: Monad (Either a)

type RWSResult r w s =
        { r :: r
        , w :: w
        , s :: s
        }
         
newtype RWS r w s a =
        RWS (RWSResult r w s -> Tuple a (RWSResult r w s))

instance functorRWS :: Functor (RWS r w s) where
        map f (RWS g) = RWS \rws -> g rws # \(Tuple x rws') -> Tuple (f x) rws'

instance applyRWS :: Monoid w => Apply (RWS r w s) where
        apply = ap

instance applicativeRWS :: Monoid w => Applicative (RWS r w s) where
        pure x = RWS \{r, s} -> Tuple x {r, w: mempty, s}

instance bindRWS :: Monoid w => Bind (RWS r w s) where
        bind (RWS g) f = RWS \rws -> g rws
                # \(Tuple x rws'@{ w }) -> runRWS (f x) rws'
                        # \(Tuple y rws''@{w:w'}) -> Tuple y rws'' { w = w <> w' }

runRWS :: forall r w s a. RWS r w s a -> RWSResult r w s -> Tuple a (RWSResult r w s)
runRWS (RWS f) = f

instance monadRWS :: Monoid w => Monad (RWS r w s)

tell :: forall r w s. w -> RWS r w s Unit
tell w = RWS \{r, s} -> Tuple unit {r, w, s} 
        
ask :: forall r w s. Monoid w => RWS r w s r
ask = RWS \{r,s} -> Tuple r {r, s, w: mempty}

get :: forall r w s. Monoid w => RWS r w s s
get = RWS \{r,s} -> Tuple s {r, s, w: mempty}

put :: forall r w s. Monoid w => s -> RWS r w s Unit
put s = RWS \{r} -> Tuple unit {r, w: mempty, s} 

type Config = {debugModeOn :: Boolean}
type Counter = Int

rwsTest :: RWS Config (Array String) Counter Unit
rwsTest = do
        tell ["test the log"]
        tell ["test the log2", "test the log3"]
        config <- ask
        tell ["the config is " <> show config]
        counter <- get
        tell ["old counter is " <> show counter]
        put $ counter + 1
        newCounter <- get
        tell ["new counter is " <> show newCounter]
        pure unit
