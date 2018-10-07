module Main where

import Prelude
import Effect (Effect)
import Story.CaptainFate as CaptainFate
import Motor.View (run)

main ∷ Effect Unit
main =
  run CaptainFate.story
