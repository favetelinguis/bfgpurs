module Data.Order where

import Prelude 

data OrderType = Limit | Market | OTC

type Order = {type :: OrderType, level :: Number}

new :: Order
new = {type: Market, level: 0.0}

-- TODO Should i have Monoid for order to get an empty order
openLimitOrder :: Number -> Order
openLimitOrder = limitOrder <<< (flip setLevel $ new)

limitOrder :: Order -> Order
limitOrder = _ {type = Limit}

-- TODO can lenses help with this?
setLevel :: Number -> Order -> Order
setLevel l  = _ {level = l}

{-
import Bar (Price(Ask, Bid))

data Trade = Order | Position {fillPrice :: Number}

-- Dont like i can call function will alternatives that does not render an position to open
-- All function would then need to handle maybe since it might not return a position
data OrderInfo = BuyOrder Price | SellOrder Price
openPosition :: OrderInfo -> String
openPosition (BuyOrder (Bid level)) = "BUY"
openPosition (SellOrder (Ask level)) = "SELL"
openPosition _ = "NotValid"
-}