
module Data.Possible
   (
     Possible(MissingData, HaveNull , HaveData)

   , possible
   , isPossibleData
   , isPossibleMissing
   , isPossibleNull
   , fromHaveData
--   , fromJust
--   , fromMaybe
--   , listToMaybe
--   , maybeToList
--   , catMaybes
--   , mapMaybe
   ,(.:??)
   ) where

import Prelude
import Data.Aeson
import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import Data.Aeson.Types(Parser)
-- ---------------------------------------------------------------------------
-- The Maybe type, and instances

-- | The 'Maybe' type encapsulates an optional value.  A value of type
-- @'Maybe' a@ either contains a value of type @a@ (represented as @'Just' a@), 
-- or it is empty (represented as 'Nothing').  Using 'Maybe' is a good way to 
-- deal with errors or exceptional cases without resorting to drastic
-- measures such as 'error'.
--
-- The 'Maybe' type is also a monad.  It is a simple kind of error
-- monad, where all errors are represented by 'Nothing'.  A richer
-- error monad can be built using the 'Data.Either.Either' type.

data Possible a = MissingData | HaveNull | HaveData a
  deriving (Eq, Ord, Show)

instance  Functor Possible  where
    fmap _ MissingData       = MissingData
    fmap _ HaveNull      = HaveNull 
    fmap f (HaveData a)  = HaveData (f a)

instance  Monad Possible where
    (HaveData x) >>= k  = k x
    MissingData  >>= _      = MissingData
    HaveNull >>= _      = HaveNull

    (HaveData _) >>  k  = k
    MissingData  >>  k      = k
    HaveNull >>  _      = HaveNull

    return              = HaveData
    fail _              = HaveNull

fromHaveData (HaveData a) = a
-- ---------------------------------------------------------------------------
-- Functions over Maybe

-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
-- value.  If the 'Maybe' value is 'Nothing', the function returns the
-- default value.  Otherwise, it applies the function to the value inside
-- the 'Just' and returns the result.
possible :: b -> (a -> b) -> Possible a -> b
possible n _ HaveNull  = n
possible _ f (HaveData x) = f x

-- | The 'isJust' function returns 'True' iff its argument is of the
-- form @Just _@.
isPossibleData         :: Possible a -> Bool
isPossibleData MissingData  = False
isPossibleData HaveNull = False
isPossibleData _        = True

-- | The 'isNothing' function returns 'True' iff its argument is 'Nothing'.
isPossibleMissing         :: Possible a -> Bool
isPossibleMissing MissingData = True
isPossibleMissing _       = False

isPossibleNull         :: Possible a -> Bool
isPossibleNull HaveNull = True
isPossibleNull _        = False

instance (ToJSON a) => ToJSON (Possible a) where
  toJSON MissingData  = Missing
  toJSON HaveNull     = Null
  toJSON (HaveData a) = toJSON a

maybeToPossible (Just a)  = HaveData a
maybeToPossible Nothing = MissingData

instance FromJSON a => FromJSON (Possible a) where
   parseJSON a = maybeToPossible <$> parseJSON a
--   parseJSON _ = fail "Not a Setting object"

(.:??) :: (FromJSON a) => Object -> Text -> Parser (Possible a)
(.:??) v s = v .:? s .!= MissingData

-- | The 'fromJust' function extracts the element out of a 'Just' and
-- throws an error if its argument is 'Nothing'.
--fromJust          :: Possible a -> a
--fromJust Nothing  = error "Maybe.fromJust: Nothing" -- yuck
--fromJust (Just x) = x
--
-- | The 'fromMaybe' function takes a default value and and 'Maybe'
-- value.  If the 'Maybe' is 'Nothing', it returns the default values;
-- otherwise, it returns the value contained in the 'Maybe'.
--fromMaybe     :: a -> Possible a -> a
--fromMaybe d x = case x of {Nothing -> d;Just v  -> v}
--
-- | The 'maybeToList' function returns an empty list when given
-- 'Nothing' or a singleton list when not given 'Nothing'.
--maybeToList            :: Possible a -> [a]
--maybeToList  Nothing   = []
--maybeToList  (Just x)  = [x]
--
-- | The 'listToMaybe' function returns 'Nothing' on an empty list
-- or @'Just' a@ where @a@ is the first element of the list.
--listToMaybe           :: [a] -> Possible a
--listToMaybe []        =  Nothing
--listToMaybe (a:_)     =  Just a
--
-- | The 'catMaybes' function takes a list of 'Maybe's and returns
-- a list of all the 'Just' values. 
--catMaybes              :: [Possible a] -> [a]
--catMaybes ls = [x | Just x <- ls]
--
-- | The 'mapMaybe' function is a version of 'map' which can throw
-- out elements.  In particular, the functional argument returns
-- something of type @'Maybe' b@.  If this is 'Nothing', no element
-- is added on to the result list.  If it just @'Just' b@, then @b@ is
-- included in the result list.
--mapMaybe          :: (a -> Possible b) -> [a] -> [b]
--mapMaybe _ []     = []
--mapMaybe f (x:xs) =
-- let rs = mapMaybe f xs in
-- case f x of
--  Nothing -> rs
--  Just r  -> r:rs
-- {-# NOINLINE [1] mapMaybe #-}
--
-- {-# RULES
-- "mapMaybe"     [~1] forall f xs. mapMaybe f xs
--                    = build (\c n -> foldr (mapMaybeFB c f) n xs)
-- "mapMaybeList" [1]  forall f. foldr (mapMaybeFB (:) f) [] = mapMaybe f
--  #-}
--
-- {-# NOINLINE [0] mapMaybeFB #-}
-- mapMaybeFB :: (b -> r -> r) -> (a -> Possible b) -> a -> r -> r
-- mapMaybeFB cons f x next = case f x of
--  Nothing -> next
--  Just r -> cons r next
