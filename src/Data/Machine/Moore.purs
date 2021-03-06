module Data.Machine.Moore where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Enum (class Enum, succ)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), swap)
import Data.Unfoldable (class Unfoldable, unfoldr)

-- TODO: linearity? Mutate the Maybe Tuple behind the scenes?
newtype Moore state output = Moore (state -> Maybe (Tuple state output))

derive instance newtypeMoore :: Newtype (Moore state output) _

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

forEach :: forall m s o. MonadRec m => Moore s o -> (o -> m Unit) -> s -> m Unit
forEach m f = tailRecM go
  where
  go s = case step s m of
    Just (Tuple s' o) -> f o *> pure (Loop s')
    Nothing           -> pure $ Done unit

repeat :: forall s. s -> Moore s s
repeat s = moore \_ -> Just (Tuple s s)

replicate :: forall o. Int -> o -> Moore Int o
replicate i o = moore \i' ->
  if i - i' > 0
    then Just (Tuple (i' - 1) o)
    else Nothing

infinite :: forall s o. (s -> Tuple s o) -> Moore s o
infinite = moore <<< map Just

stateOnly :: forall s. (s -> Maybe s) -> Moore s s
stateOnly = moore <<< map (map double)

iterate :: forall s. (s -> s) -> Moore s s
iterate = moore <<< map (Just <<< double)

-- this is still faster than upTo for Ints
upToN :: Int -> Moore Int Int
upToN i = moore \i' ->
  if i == i'
    then Nothing
    else let i'' = i' + 1 in Just $ double i''

upTo :: forall s. Enum s => s -> Moore s s
upTo s =
  -- optimize by hoisting TC dictionary lookups
  let succ' = succ :: s -> Maybe s
      eq' = eq :: s -> s -> Boolean
  in moore \s' ->
    if eq' s s'
      then Nothing
      else double <$> succ' s'

--TODO: cycle from Data.List.Lazy

-- | Utility function
double :: forall a. a -> Tuple a a
double x = Tuple x x
