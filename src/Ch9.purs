module Ch9 where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe (..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, show, ($), (==))

test :: Effect Unit
test = do
  log $ show $ ATrue <> ATrue
  log $ show $ ATrue <> AFalse
  log $ show $ AFalse<> AFalse
  log $ show $ mempty <> ATrue == ATrue
  log $ show $ mempty <> AFalse == ATrue
  verifyAndBoolSemigroup 
  verifyAndBoolMonoid

class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

class Semigroup a <= Monoid a where
  mempty :: a

data AndBool = AFalse | ATrue
derive instance egAndBool :: Eq AndBool 
derive instance genericAndBool :: Generic AndBool _

instance showAndBool :: Show AndBool where
  show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _ = AFalse

instance monoidAndBool :: Monoid AndBool where
  mempty = ATrue

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
  log "Verify AndBool Semigroup Laws (1 test)"
  log $ show $ (AFalse <> (AFalse <> ATrue)) == ((AFalse <> AFalse) <> ATrue)

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
  log "Verifying AndBool Monoid Laws (2 tests)"
  log $ show $ mempty <> AFalse == AFalse
  log $ show $ mempty <> ATrue == ATrue

data OrBool = OFalse | OTrue
derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _

instance showOrBool :: Show OrBool where
  show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
  append OFalse OFalse = OTrue
  append _ _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse

newtype First a = First (Maybe a)
newtype Last a = Last (Maybe a)

instance semigroupFirst :: Semigroup (First a) where
  append (First Nothing) last = last
  append  first _ = first

instance monoidFist :: Monoid (First a) where
  mempty = First Nothing

instance semigroupLast :: Semigroup (Last a) where
  append first (Last Nothing) = first
  append _ last = last

instance monoidLast :: Monoid (Last a) where
  mempty = Last Nothing