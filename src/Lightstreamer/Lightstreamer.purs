module Lightstreamer (Subscription, LightstreamerClient, newSubscription, addSubscription, removeSubscription, connect, disconnect, newClient) where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Ref (STRef, new)
import Data.Function.Uncurried (Fn1, Fn2, Fn4, runFn1, runFn2, runFn4)
import Effect (Effect)
import Effect.Ref (Ref)
import Prim.Row (class Union)

-- How to specify no arg function that is a void
type ConnectionListner
  = { onStatusChange :: String -> Effect Unit
    , onServerError :: Number -> String -> Effect Unit
    }

type FieldName
  = String

type FieldPossition
  = Number

type FieldValue
  = String

-- Do i need row type poly here, only care about these two functions?
type ItemUpdate
  = forall r.
    { forEachChangedField :: FieldName -> FieldPossition -> FieldValue -> Effect Unit
    , forEachField :: FieldName -> FieldPossition -> FieldValue -> Effect Unit
    | r
    }

type SubscriptionListner
  = { onSubscription :: Effect Unit
    , onUnsubscription :: Effect Unit
    , onSubscriptionError :: Number -> String -> Effect Unit
    , onItemUpdate :: ItemUpdate -> Effect Unit
    }

-- Do dataimport actually need to export something from JS or is it just a name?
foreign import data Subscription :: Type

-- Should i make this type more complex and include connect disconnect subscribe etc inside this type
-- or is it better to do as i have done to breake them out as functions on the JS side?
-- or maybe I should create the client as a let binding on the JS side and only mutate the client object from purescript??
foreign import data LightstreamerClient :: Type

foreign import newImpl :: Fn4 String String String String LightstreamerClient

newClient :: forall r. String -> String -> String -> String -> (ST r (STRef r LightstreamerClient))
newClient url usr pwd listner = new $ client
  where
  client :: LightstreamerClient
  client = runFn4 newImpl url usr pwd listner

foreign import newSubscriptionImpl :: Fn4 String (Array String) (Array String) SubscriptionListner Subscription

newSubscription :: String -> Array String -> Array String -> SubscriptionListner -> Subscription
newSubscription m i f l = runFn4 newSubscriptionImpl m i f l

foreign import connectImpl :: Fn1 LightstreamerClient (Effect Unit)

connect :: LightstreamerClient -> Effect Unit
connect = runFn1 connectImpl

-- TODO catch errors in Either and use Effect Either String Unit?
foreign import disconnectImpl :: Fn1 LightstreamerClient (Effect Unit)

disconnect :: LightstreamerClient -> Effect Unit
disconnect c = runFn1 disconnectImpl c

foreign import addSubscriptionImpl :: Fn2 LightstreamerClient Subscription (Effect Unit)

addSubscription :: LightstreamerClient -> Subscription -> Effect Unit
addSubscription c s = runFn2 addSubscriptionImpl c s

foreign import removeSubscriptionImpl :: Fn2 LightstreamerClient Subscription (Effect Unit)

removeSubscription :: LightstreamerClient -> Subscription -> Effect Unit
removeSubscription c s = runFn2 removeSubscriptionImpl c s

{- One way to handle mutable object?
foreign import new :: a -> Ref a
foreign import connect :: (a -> Effect Unit ) -> Ref a -> Effect Unit
foreign import close :: (a -> Effect Unit ) -> Ref a -> Effect Unit
foreign import subscribe :: (a -> Effect Unit ) -> Ref a -> Effect Unit
foreign import unsubscribe :: (a -> Effect Unit ) -> Ref a -> Effect Unit
foreign import swap :: a -> Ref a -> Effect Unit
-}
