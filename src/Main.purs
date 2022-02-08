module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Ig (setupIg)
import Lightstreamer (runner)


-- TODO read a config file with login details and create a monad transformer starck with 
-- ReaderT and Aff or is it Effect?
main :: Effect Unit
main = launchAff_ do
  liftEffect $ log "Startar main"
  -- Send in config to setupIg
  conn <- liftEffect setupIg
  -- I also need some state Monad to reuse each iteration
  -- Fold over a list with bars -- Look for setup
  -- Send in a price tick -- Look for entry or exit
  -- liftEffect $ log conn
  liftEffect $ runner "HOHOHO"