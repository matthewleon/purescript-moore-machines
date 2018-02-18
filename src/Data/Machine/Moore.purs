module Data.Machine.Moore where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), swap)
import Data.Unfoldable (class Unfoldable, unfoldr)

newtype Moore s o = Moore (s -> Maybe (Tuple s o))

derive instance newtypeMoore :: Newtype (Moore s o) _

instance functorMoore :: Functor (Moore state) where
  map f (Moore t) = Moore $ map (map f) <$> t

instance applyMoore :: Apply (Moore state) where
  apply f g = Moore \s -> case step s g of
    Just (Tuple s' o) -> (_ <@> o) <$> step s' f
    Nothing -> Nothing

instance applicativeMoore :: Applicative (Moore state) where
  pure o = Moore \s -> Just $ Tuple s o

-- bind, monad, and friends
-- foldable, etc.

moore :: forall s o. (s -> Maybe (Tuple s o)) -> Moore s o
moore = Moore

step :: forall s o. s -> Moore s o -> Maybe (Tuple s o)
step state (Moore transition) = transition state

toUnfoldable :: forall u s o. Unfoldable u => Moore s o -> s -> u o
toUnfoldable m = unfoldr unfoldStep
  where
  unfoldStep b = swap <$> step b m