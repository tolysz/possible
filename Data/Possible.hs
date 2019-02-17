{-# LANGUAGE DeriveDataTypeable, DeriveGeneric  #-}

module Data.Possible
   ( Possible(..)
   , possible
   , isPossibleData
   , isPossibleMissing
   , isPossibleNull
   , fromHaveData
   ) where

import Prelude
import Control.Applicative
import Data.Maybe
import GHC.Generics
import Data.Typeable
import qualified Control.Monad.Fail as Fail

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
    MissingData  >>  _  = MissingData
    (HaveData _) >>  k  = k
    return              = HaveData

instance Fail.MonadFail Possible where
    fail _              = HaveNull

fromHaveData :: Possible a -> a
fromHaveData HaveNull     = error "Maybe.fromHaveData: HaveNull"
fromHaveData MissingData  = error "Maybe.fromHaveData: MissingData"
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
