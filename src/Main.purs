module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Web.DOM.NonElementParentNode (getElementById)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Exception (throw)
import React.Basic.Hooks (ReactComponent, element, reactComponent, useState)
import React.Basic.DOM as D
import React.Basic.Hooks as R



main :: Effect Unit
main = do
  log "Rendering address book component"
  -- Get window object
  w <- window
  -- Get window's HTML document
  doc <- document w
  -- Get "container" element in HTML
  ctr <- getElementById "container" $ toNonElementParentNode doc
  case ctr of
    Nothing -> throw "Container element not found."
    Just c -> do
      -- Create AddressBook react component
      addressBookApp <- mkAddressBookApp
      let
        -- Create JSX node from react component. Pass-in empty props
        app = element addressBookApp {}
      -- Render AddressBook JSX node in DOM "container" element
      D.render app c

mkAddressBookApp :: Effect (ReactComponent {})
mkAddressBookApp =
  reactComponent
    "AddressBookApp"
    (\props -> pure $ D.text "Hi! I'm an address book")