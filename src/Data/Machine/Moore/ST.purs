module Data.Machine.Moore.ST where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)

newtype Moore h s o = Moore (s -> Eff (st :: ST h) (Step h s o))

foreign import data Step :: Type -> Type -> Type -> Type

foreign import mkStep :: forall h s o. s -> o -> Eff (st :: ST h) (Step h s o)

foreign import halted :: forall h s o. Step h s o -> Eff (st :: ST h) Boolean

foreign import output :: forall h s o. Step h s o -> Eff (st :: ST h) o

moore :: forall h s o. (s -> Eff (st :: ST h) (Step h s o)) -> Moore h s o
moore = Moore

step :: forall h s o. s -> Moore h s o -> Eff (st :: ST h) (Step h s o)
step state (Moore transition) = transition state
