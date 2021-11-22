module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Ig (setupIg)



main :: Effect Unit
main = launchAff_ do
  liftEffect $ log "Startar main"
  conn <- liftEffect setupIg
  liftEffect $ log conn