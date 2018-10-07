module Motor.View.Util
  ( addHead
  , hideCursor
  , innerWidth
  , innerHeight
  ) where

import Prelude
import Effect (Effect)
import Web.DOM.Document (Document)

foreign import addHead ∷ Document → String → Effect Unit

foreign import hideCursor ∷ Document → Effect Unit

foreign import innerWidth ∷ Effect Int

foreign import innerHeight ∷ Effect Int
