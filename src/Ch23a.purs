module Ch23a where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, joinFiber, killFiber, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)
import Effect.Exception (error)

test :: Effect Unit
test = launchAff_ do
    ttAVar <- AVar.empty
    clockFiber <- forkAff $ clock ttAVar
    bombFiber <- forkAff $ bomb ttAVar 5
    AVar.put Tick ttAVar
    joinFiber bombFiber
    killFiber (error "Exploded") clockFiber

data TickTock = Tick | Tock
derive instance eqTickTock :: Eq TickTock

tick :: AVar TickTock -> Aff Unit
tick ttAVar = do
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tick ttAVar
  tick ttAVar

tock :: AVar TickTock -> Aff Unit
tock ttAVar = do
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tock ttAVar
  tock ttAVar

clock :: AVar TickTock -> Aff Unit
clock ttAVar = do
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tock ttAVar
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tick ttAVar
  clock ttAVar

data BombState = WaitingTick | WaitingTock

bomb :: AVar TickTock -> Int -> Aff Unit
bomb ttAVar detinationCount = go 0 WaitingTick where
  go count state = do
    if count == detinationCount then log "BOOM!"
    else do
      delay (Milliseconds 500.0)
      tt <- AVar.read ttAVar
      case state of
        WaitingTick ->
            if tt == Tick then log "Tick" *> go (count + 1) WaitingTock
            else go count state
        WaitingTock -> 
            if tt == Tock then log "Tock" *> go (count + 1) WaitingTick
            else go count state
  -- Wait 500ms
  -- Get the current value
  -- If the current value is not the same as prev value decrese count med 1
  -- om count is 0 terminate program else recur
  