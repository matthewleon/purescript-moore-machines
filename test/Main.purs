module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Machine.Moore (forEach, upToN)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Quick stack safety test."
  let counter = upToN 1000000
  forEach counter logShow 0
