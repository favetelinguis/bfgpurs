module Ch6 where
  
  
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show This

data SomeType = This | That | TheOther | AndYetAnother

derive instance eqSomeType :: Eq SomeType
derive instance ordSomeType :: Ord SomeType
derive instance genericSomeType :: Generic SomeType _
instance showSomeType :: Show SomeType where
  show = genericShow

newtype FirstName = FirstName String
derive instance newTypeFirstName :: Newtype FirstName _

newtype LastName = LastName String
derive instance newTypeLastName :: Newtype LastName _

glueNames :: forall a b
  . Newtype a String 
  => Newtype b String
  => String -> a -> b -> String
glueNames between n1 n2 = unwrap n1 <> between <> unwrap n2

lastNameFirst :: LastName -> FirstName -> String
lastNameFirst = glueNames ", "

data Address = Address 
  { street1 :: String 
  , street2 :: String 
  , city :: String
  , state :: String 
  , zip :: String
}

fullName :: FirstName -> LastName -> String
fullName = glueNames ""

class HasAddress a where
  getAddress :: a -> Address

newtype Person = Person
  {name:: String
  , age :: Int
  , address :: Address
  }

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

newtype Ceo = Ceo Person
newtype Janitor = Janitor Person

--derive instance newTypeCeo :: Newtype Ceo _
--derive instance newTypeJanitor :: Newtype Janitor _

derive newtype instance hasAddressCeo :: HasAddress Ceo
derive newtype instance hasAddressJanitor :: HasAddress Janitor

{-
instance hasAddressCeo :: HasAddress Ceo where
  getAddress = getAddress <<< unwrap

instance hasAddressJanitor :: HasAddress Janitor where
  getAddress = getAddress <<< unwrap
-}

data Overlap = Overlap

--instance overlapShow1 :: Show Overlap where
--  show = "Overlap 1"

--instance overlapShow2 :: Show Overlap where
--  show = "Overlap 2"