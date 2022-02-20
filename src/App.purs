module App where 

import Prelude

import Control.Monad.Error.Class (class MonadError, try)
import Control.Monad.Except (ExceptT, runExcept, runExceptT)
import Control.Monad.RWS (RWSResult(..), RWST, get, put, runRWST, tell)
import Control.Monad.Writer (class MonadTell)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), fromRight, hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, launchAff_, message)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Foreign.Generic (F, decodeJSON)
import Ig.Client as Ig
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Record (merge)
import Undefined (undefined)

type AppStack e r w s a = ExceptT e (RWST r w s Effect) a
type Session = {apikey :: String}
type StreamsStatus = {connection :: String, account :: String, market :: String, trade :: String}
type AppState = {session :: Maybe Session, streamsStatus:: StreamsStatus, subscriptions :: String, markets :: String, account :: String, orders :: String }
type Config = {user :: String, password :: String}
type AppM = AppStack String Config String AppState Unit
type StackResult = RWSResult AppState (Either String Unit) String
type AppEffects = {log :: String, state :: AppState, result :: Maybe Unit}
type AppResult = Tuple (Maybe String) AppEffects

runApp :: AppState -> Config -> AppM -> Effect AppResult
runApp s c ap = results <$> (runRWST m c s) where
  m = runExceptT ap

results :: StackResult -> AppResult 
results (RWSResult state (Left err) l) = Tuple (Just err) {log: l, state, result: Nothing}  
results (RWSResult state (Right result) l) = Tuple Nothing {log: l, state, result: Just result}  

-- log :: ∀ m. MonadTell String m => String -> m Unit
-- log s = tell $ s <> "\n"

app :: AppM
app = do
  log "Starting App..."
  _ <- get
  log "Incremented State"
  pure unit

createConfig :: Config
createConfig = {user: "user", password: "password"}

createStreamStatus :: StreamsStatus
createStreamStatus = {connection: "", account: "", market: "", trade: ""}

createAppState :: AppState
createAppState = {session: Nothing, streamsStatus: createStreamStatus, subscriptions: "subs", markets: "markets", account: "account", orders: "orders"} 

main :: Effect Unit
main = do
  conf <- (\e -> lmap  message e) <$> (try $ readTextFile ASCII "config.json")
  decoded <- pure $ conf >>= \s -> lmap show (runExcept $ decodeJSON s)
  requestAVar <- AVar.empty
  case decoded of
        Right fileData -> startApp fileData requestAVar
        Left e -> log e

startApp :: { | Ig.ConnectionDetails ()} -> AVar.AVar String -> Effect Unit
startApp conf avar = launchAff_ do
        Ig.runClientM (Ig.Config $ merge conf {inRequest: avar}) Ig.client

{-
import Data.Tuple (Tuple(..))
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Reader.Trans (class MonadAsk)
import Control.Monad.Writer.Trans (class MonadTell, tell)
import Control.Monad.State.Trans (class MonadState, state)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Control.Monad.Except.Trans (ExceptT, throwError, runExceptT)
import Control.Monad.State.Trans (StateT, get, put, runStateT)
import Control.Monad.Writer.Trans (WriterT, class MonadTell, tell, runWriterT)
import Effect (Effect)
import Effect.Console as Console

type AppEffects =
       { log :: String
       , state :: Int
       , result :: Maybe Unit
       }

type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a
type AppM = AppStack String String Int Unit
type StackResult = Tuple (Tuple (Either String Unit) String) Int
type AppResult = Tuple (Maybe String) AppEffects

app :: AppM
app = do
        log "Starting App..."
        n <- get
        when (n == 0) $ void $ throwError "WE CANNOT HAVE 0 STATE"
        put $ n + 1
        log "Incremented State"
        pure unit

log :: forall m. MonadTell String m => String -> m Unit
log s = tell $ s <> "\n"

runApp :: Int 
        -> AppM
        -> Effect AppResult
runApp s = map results
       <<< flip runStateT s 
       <<< runWriterT 
       <<< runExceptT

results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s)
        = Tuple (Just err) {log: l, state: s, result: Nothing}
results (Tuple (Tuple (Right _) l) s)
        = Tuple Nothing {log: l, state: s, result: Just unit}

main :: Effect Unit
main = do
        result1 <- runApp 0 app
        Console.log $ show result1
        result2 <- runApp 99 app
        Console.log $ show result2

        

newtype WriterT w m a = WriterT (m (Tuple a w)) -- WriterT result is in the monad below

newtype ReaderT r m a = ReaderT (r -> m a)

runReaderT :: forall r m a. ReaderT r m a -> r -> m a
runReaderT (ReaderT mf) = mf

runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w) 
runWriterT (WriterT mx) = mx

instance functorReaderT :: Functor m => Functor (ReaderT r m) where
        map f (ReaderT mg) = ReaderT \r -> f <$> mg r

instance functorWriterT :: Functor m => Functor (WriterT w m) where
        map f (WriterT mx) = WriterT $ mx <#> \(Tuple x w) -> Tuple (f x) w

instance applyReaderT :: Apply m => Apply (ReaderT r m) where
        apply (ReaderT fmf) (ReaderT fmx) = ReaderT \r -> fmf r <*> fmx r

instance applyWriterT ::(Semigroup w, Monad m) => Apply (WriterT w m) where
        apply (WriterT mf) (WriterT mx) = WriterT do
           Tuple f w1 <- mf
           Tuple x w2 <- mx
           pure $ Tuple (f x) (w1 <> w2)

instance applicativeReaderT :: Monad m => Applicative (ReaderT r m) where
        pure = lift <<< pure

instance applicativeWriterT :: (Monoid w, Monad m) => Applicative (WriterT w m) where
        pure x = WriterT $ pure $ Tuple x mempty

instance bindWriterT :: (Semigroup w, Monad m) => Bind (WriterT w m) where
        bind (WriterT mx) f = WriterT do
           Tuple x w1 <- mx
           Tuple y w2 <- runWriterT $ f x
           pure $ Tuple y $ w1 <> w2

instance bindReaderT :: Bind m => Bind (ReaderT r m) where
        bind (ReaderT fmx) f = ReaderT \r -> do
           x <- fmx r
           runReaderT (f x) r


instance monadReaderT :: Monad m => Monad (ReaderT r m)

instance monadTransReaderT :: MonadTrans (ReaderT r) where
        lift = ReaderT <<< const

instance monadAskReaderT :: Monad m => MonadAsk r (ReaderT r m) where
        ask = ReaderT pure

instance monadTellReaderT :: MonadTell w m => MonadTell w (ReaderT r m) where
        tell = lift <<< tell

instance monadStateReaderT :: MonadState s m => MonadState s (ReaderT r m) where
        state = lift <<< state

-}