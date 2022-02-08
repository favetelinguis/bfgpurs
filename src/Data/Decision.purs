module Data.Decision where

import Effect.Aff (Aff)

data InformationUpdate = Market | Account | Order | Position
type DecisionM = Aff Decision

data Decision = Setup | Entry | Exit | Nothing

decide :: InformationUpdate -> Decision
decide _ = Setup