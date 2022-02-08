module Ch7b where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ toCSV 
    (Person 
      { age: Age 3
      , name: FullName "Full Name"
      , occupation: Lawyer
      }) == CSV "Full Name,3,Lawyer"
  let person = (Person
    { name: FullName "Sue Smith"
    , age: Age 23
    , occupation: Doctor
    })
  log $ show $ (toCSV person # fromCSV) == Just person

newtype CSV = CSV String
derive instance newTypeCSV :: Newtype CSV _
derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV 

newtype FullName = FullName String
instance showFullName :: Show FullName where
  show (FullName s) = s
derive newtype instance eqFullName :: Eq FullName
newtype Age = Age Int
derive instance newTypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age
derive newtype instance eqAge :: Eq Age
data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance eqOccupation :: Eq Occupation
derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow
data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation }

instance toCSVPerson :: ToCSV Person where
  toCSV (Person {name, age, occupation}) = CSV $ show name <> "," <> show age <> "," <> show occupation

toOccupation :: String -> Maybe Occupation
toOccupation "Doctor" = Just Doctor
toOccupation "Dentist" =  Just Dentist
toOccupation "Unemployed" =  Just Unemployed
toOccupation _ =  Nothing

class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV str) = case split (Pattern ",") str of
    [name, age, occupation] -> do
      age' <- fromString age
      occupation' <- toOccupation occupation
      pure $ Person
        { name: FullName name
        , age: Age age'
        , occupation: occupation'
        }
    _ -> Nothing

derive instance eqPerson :: Eq Person