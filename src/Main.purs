module Main where

import Prelude (Unit, bind, pure, unit)
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Function.Eff
import Graphics.D3.Base
import Debug.Trace
import DOM

import Graphics.D3.Selection

import Graphics.D3.Examples.ForceLayout1

main :: forall e. Eff (d3 :: D3, console :: CONSOLE, dom :: DOM | e) Unit
main = do
  -- trace "Testing" \_ ->
  -- traceAny svg \_ ->
  -- pure unit
  mainD3
  log "Hello sailor!"
