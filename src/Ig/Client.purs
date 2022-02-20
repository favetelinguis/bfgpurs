module Ig.Client where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (StateT, get, runStateT)
import Data.Maybe (Maybe(..))
import Effect.AVar (AVar)
import Effect.Aff (Aff, forkAff)
import Effect.Class.Console (log)
import Undefined (undefined)

type ConnectionDetails r = ( user :: String, pwd :: String, baseUrl :: String, accountId :: String, apiKey:: String | r )
type ConfigRow r = (inRequest :: AVar String | r)
newtype Config = Config { | ConnectionDetails (ConfigRow ()) }
instance showConfig :: Show Config where
  show (Config { user, pwd, baseUrl, accountId, apiKey }) = show { user, pwd, baseUrl, accountId, apiKey }
type State = { session :: Maybe String }
type ClientM a = ReaderT Config (StateT State Aff) a

-- Create a forever fiber
-- First read config from file in aff

runClientM :: Config -> ClientM Unit -> Aff Unit
runClientM config = void <<< forkAff <<< flip runStateT { session: Nothing } <<< flip runReaderT config

client :: ClientM Unit
client = forever do
    {session} <- get
    Config {user} <- ask
    case session of
        Just s -> log "Has session, just execute request"
        Nothing -> log $ "Dont have session, create session then do request with config: " <> user

-- Define Request and Response types and also
-- data IgRequests = CreateSession Request 
                -- | DeleteSession Request