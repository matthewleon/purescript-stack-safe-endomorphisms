module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Monoid (power)
import Data.Monoid.Endo.StackSafe (applyEndo, endo)
import Test.Assert (ASSERT, assert)

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  log "applyEndo should be stack-safe"
  let incrementChain = power (endo (_ + 1)) 1000000
  assert $ applyEndo incrementChain 0 == 1000000
