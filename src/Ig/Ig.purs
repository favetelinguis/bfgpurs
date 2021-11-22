module Ig where

import Prelude

import Data.Map (Map)
import Effect (Effect)
import Effect.Class.Console (log)

type MarketName = String
data ItemType = I1 | I2
type Items = Array ItemType
data Session = NoSession | Created {account :: String}
data StreamConnection = Connected | Disconnected
data SubscriptionStatus = Subscribed (Map MarketName Items) | NotSubscribed
data StreamClient = StreamClient
type Subscription = {market :: MarketName}

createSession :: Effect Session
createSession = do 
    log "Creating session OK"
    pure $ Created {account : "MYACCOUNT"}

createClient :: Session -> StreamClient
createClient s = StreamClient

-- Check client status
connect :: StreamClient -> Session -> Effect Unit 
connect _ _ = do log "Connection OK"

-- Check client status
disconnect :: StreamClient -> Effect Unit 
disconnect _ = do log "Disconnection OK"

-- Check client status and add subscription to Map
subscribe :: StreamClient -> Subscription -> Effect Unit
subscribe _ _ = do log "Subscribe OK"

-- Check client status
unsubscribe :: StreamClient -> Subscription -> Effect Unit
unsubscribe _ _ = do log "Unsubscribe OK"

setupIg :: Effect String
setupIg = do
  s <- createSession
  let c = createClient s -- How to singnal that c is actually mutable???
  connect c s
  let sub = {market: "NARKET"}
  subscribe c sub
  unsubscribe c sub
  disconnect c

  pure "Connected to IG"