{-# LANGUAGE DeriveDataTypeable, DeriveGeneric  #-}

module Data.Possible
   ( Possible(..)
   , possible
   , isPossibleData
   , isPossibleMissing
   , isPossibleNull
   , fromHaveData
   -- ,(.:??)
   ) where

import Prelude
import Control.Applicative
import Data.Maybe
import GHC.Generics
import Data.Typeable

data Possible a = HaveNull | MissingData | HaveData a
  deriving (Show, Generic, Typeable)

instance  Functor Possible where
    fmap _ HaveNull      = HaveNull
    fmap _ MissingData   = MissingData
    fmap f (HaveData a)  = HaveData (f a)

instance Applicative Possible  where
    pure = HaveData
    (HaveData f) <*> (HaveData x) = HaveData (f x)
    MissingData  <*> MissingData  = MissingData
    (HaveData _) <*> MissingData  = MissingData
    MissingData  <*> (HaveData _) = MissingData
    _            <*> _            = HaveNull

instance  Monad Possible where
    HaveNull     >>= _  = HaveNull
    MissingData  >>= _  = MissingData
    (HaveData x) >>= k  = k x

    HaveNull     >>  _  = HaveNull
    MissingData  >>  k  = k
    (HaveData _) >>  k  = k

    return              = HaveData
    fail _              = HaveNull

fromHaveData :: Possible a -> a
fromHaveData (HaveData a) = a

possible :: b -> b -> (a -> b) -> Possible a -> b
possible n _ _ HaveNull     = n
possible _ m _ MissingData  = m
possible _ _ f (HaveData x) = f x

isPossibleData :: Possible a -> Bool
isPossibleData MissingData  = False
isPossibleData HaveNull     = False
isPossibleData _            = True

isPossibleMissing :: Possible a -> Bool
isPossibleMissing MissingData = True
isPossibleMissing _           = False

isPossibleNull :: Possible a -> Bool
isPossibleNull HaveNull = True
isPossibleNull _        = False

{-
Move it to Aeson

instance (ToJSON a) => ToJSON (Possible a) where
  toJSON MissingData  = Missing
  toJSON HaveNull     = Null
  toJSON (HaveData a) = toJSON a

maybeToPossible (Just a)  = HaveData a
maybeToPossible Nothing = MissingData

instance FromJSON a => FromJSON (Possible a) where
   parseJSON a = maybeToPossible <$> parseJSON a

(.:??) :: (FromJSON a) => Object -> Text -> Parser (Possible a)
(.:??) v s = v .:? s .!= MissingData

-}
