module Ch21 where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadTrans, ask, lift)
import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer (class MonadTell, WriterT, runWriterT, tell)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  result1 <- runApp 0 app
  log $ show result1
  result2 <- runApp 99 app
  log $ show result2

newtype StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: forall s m a. StateT s m a -> (s -> m (Tuple a s))
runStateT (StateT f) = f

instance functorStateT :: Functor m => Functor (StateT s m) where
  map f (StateT mg) = StateT \s -> mg s <#> \(Tuple x s') -> Tuple (f x) s'

instance applyStateT :: Monad m => Apply (StateT s m) where
  apply :: forall a b. StateT s m (a -> b) -> StateT s m a -> StateT s m b
  apply (StateT fmf) (StateT fmx) = StateT \s -> do
    (Tuple f s') <- fmf s
    (Tuple x s'') <- fmx s'
    pure $ Tuple (f x) s''

instance applicativeStateT :: Monad m => Applicative (StateT s m) where
  pure x = StateT \s -> pure $ Tuple x s

instance bindStateT :: Monad m => Bind (StateT s m) where
  bind :: forall a b. StateT s m a -> (a -> StateT s m b) -> StateT s m b
  bind (StateT fmx) f = StateT \s -> fmx s >>= \(Tuple x s') -> runStateT (f x) s'

instance monadStateT :: Monad m => Monad (StateT s m)

instance monadStateStateT :: Monad m => MonadState s (StateT s m) where
  state f = StateT $ pure <<< f

instance monadAskStateT :: MonadAsk r m => MonadAsk r (StateT s m) where
  ask :: StateT s m r
  ask = lift ask

instance monadTellStateT :: MonadTell r m => MonadTell r (StateT s m) where
  tell = lift <<< tell

instance monadTransReaderT :: MonadTrans (StateT s) where
  lift mx = StateT \s -> mx <#> \r -> Tuple r s

instance monadThrowStateT :: MonadThrow e m => MonadThrow e (StateT s m) where
  throwError = lift <<< throwError

instance monadErrorStateT :: MonadError e m => MonadError e (StateT s m) where
  catchError  (StateT fmx) f = StateT \s -> catchError (fmx s) \e -> runStateT (f e) s

type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a

type AppM = AppStack String String Int Unit

type StackResult = (Tuple (Tuple (Either String Unit) String) Int)

type AppEffects = {log :: String, state :: Int, result :: Maybe Unit}

type AppResult = Tuple (Maybe String) AppEffects

runApp :: Int -> AppM -> Effect AppResult 
runApp st = (results <$> _) <<< flip runStateT st <<< runWriterT <<< runExceptT

results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s) = Tuple (Just err) { log: l, state: s, result: Nothing} 
results (Tuple (Tuple (Right result) l) s) = Tuple Nothing { log: l, state: s, result: Just result} 

app :: AppM
app = do
  tell "Starting App..."
  s <- get
  when (s == 0) $ void $ throwError "WE CAN NOT HAVE ZERO STATE"
  put $ s + 1
  tell "Incremented State"
  pure unit