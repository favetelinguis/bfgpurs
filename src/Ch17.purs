module Ch17 
( Age (..)
, Either(..)
, FamilyAges(..)
, FamilyAgesRow
, Validation(..)
, createFamilyAges
, test) where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ (+) <$> Just 21 <*> Just 21
  log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
  log $ show $ pure (+) <*> Just 21 <*> Just 21
  log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
  log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
  log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
  log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)
  log $ show $ createFamilyAges
    { fatherAge: Age 40, motherAge: Age 30, childAge: Age 10 }
  log $ show $ createFamilyAges
    { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0 }
  log $ show $ createFamilyAges
    { fatherAge: Age 4, motherAge: Age 3, childAge: Age 10 }
  log $ show $ createFamilyAges
    { fatherAge: Age 40, motherAge: Age 30, childAge: Age 100 }
  log $ show $ createFamilyAges
    { fatherAge: Age 40, motherAge: Age 3, childAge: Age 0 }

data Maybe a = Nothing | Just a
derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
  apply (Just f) x = f <$> x
  apply Nothing _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure = Just
  
data Either a b = Left a | Right b
derive instance genericEither :: Generic (Either a b) _
derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance functorEither :: Functor (Either a)
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right y) = Right $ g y

instance applyEither :: Apply (Either a) where
  apply (Right f) x = f <$> x
  apply (Left f) _ = Left f

instance applicativeEither :: Applicative (Either a) where
  pure = Right

newtype Validation err result = Validation (Either err result)
derive instance newtypeValidation :: Newtype (Validation err result) _
derive newtype instance functorValidation :: Functor (Validation err)
derive newtype instance bifunctorValidation :: Bifunctor Validation
derive instance eqValidation :: (Eq err, Eq result) => Eq (Validation err result)
derive instance ordValidation :: (Ord err, Ord result) => Ord (Validation err result)

instance applyValidation :: Semigroup err => Apply (Validation err) where
  apply (Validation (Left err1)) (Validation (Left err2)) = Validation $ Left (err1 <> err2)
  apply (Validation (Left err)) _ = Validation $ Left err
  apply (Validation (Right f)) x = f <$> x

instance applicativeValidation :: Semigroup err => Applicative (Validation err) where
  pure = Validation <<< Right

derive instance genericValidation :: Generic (Validation err result) _
instance showValidation :: (Show err, Show result) => Show (Validation err result) where
  show = genericShow

newtype Age = Age Int
newtype FullName = FullName String
type FamilyAgesRow r = 
  (fatherAge :: Age, motherAge :: Age, childAge :: Age | r)
type FamilyNamesRow r = 
  (fatherName :: FullName , motherName :: FullName , childName :: FullName | r)
newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ())}

derive instance genericAge :: Generic Age _
instance showAge :: Show Age where show = genericShow
derive instance genericFullName :: Generic FullName _
instance showFullName :: Show FullName where show = genericShow
derive instance genericFamily :: Generic Family _
instance showFamily :: Show Family where show = genericShow

newtype FamilyAges = FamilyAges { | FamilyAgesRow () }
derive instance genericFamilyAges :: Generic FamilyAges _ 
instance showFamilyAges :: Show FamilyAges where
  show = genericShow

newtype UpperAge = UpperAge Int
newtype LowerAge = LowerAge Int

validateAge :: LowerAge -> UpperAge -> Age -> String -> Validation (Array String) Age
validateAge (LowerAge lower) (UpperAge upper) (Age age) who
  | age > upper = Validation $ Left [ who <> " is to old" ]
  | age < lower = Validation $ Left [ who <> " is to young" ]
  | otherwise = Validation $ Right $ Age age

createFamilyAges :: { | FamilyAgesRow ()} -> Validation (Array String) FamilyAges
createFamilyAges { fatherAge , motherAge, childAge } = 
  FamilyAges <$> ({fatherAge: _, motherAge: _, childAge: _} 
    <$> (validateAge (LowerAge 18) (UpperAge 100) fatherAge "Father") 
    <*> (validateAge (LowerAge 18) (UpperAge 100) motherAge "Mother")
    <*> (validateAge (LowerAge 1) (UpperAge 18) childAge "Child"))