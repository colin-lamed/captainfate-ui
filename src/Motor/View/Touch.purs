module Motor.View.Touch
  ( handleTouchStart
  , handleTouchMove
  , handleTouchEnd
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, forkAff, delay, launchAff_, makeAff)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Web.Event.Event (preventDefault)
import Web.TouchEvent.Touch as Touch
import Web.TouchEvent.TouchEvent as TouchEvent
import Web.TouchEvent.TouchList as TouchList


-- click is valid if is released within 0.6s
-- long (right) click is valid if held longer than 0.8s
handleTouchStart ∷ TouchEvent.TouchEvent → Ref Boolean → Ref Boolean → Aff (Tuple Int Int)
handleTouchStart te clickRef rightclickRef =
  makeAff \callback → do launchAff_ $ handleTouchStart' te clickRef rightclickRef (\res → callback (Right res))
                         mempty

handleTouchStart' ∷ TouchEvent.TouchEvent → Ref Boolean → Ref Boolean → (Tuple Int Int → Effect Unit) → Aff Unit
handleTouchStart' te clickRef rightclickRef onRightClick = do
  liftEffect $ log "touchstart"
  liftEffect $ preventDefault $ TouchEvent.toEvent te
  if numTouches te > 1
    then pure unit
    else do
      liftEffect $ Ref.write true clickRef
      liftEffect $ Ref.write true rightclickRef
      _ ← forkAff $ do
        delay (Milliseconds 800.0)
        rightclick ← liftEffect $ Ref.read rightclickRef
        if rightclick
          then do
            let touches = TouchEvent.touches te
            case map getTouchXY $ extractFirstTouch touches of
              Just xy → liftEffect $ onRightClick xy
              Nothing → pure unit
          else do
            liftEffect $ log "rightclick cancelled"
            pure unit
      _ ← forkAff $ do
        delay (Milliseconds 600.0)
        liftEffect $ Ref.write false clickRef
      pure unit

handleTouchMove ∷ TouchEvent.TouchEvent → Ref Boolean → Ref Boolean → (Tuple Int Int → Effect Unit) → Effect Unit
handleTouchMove te clickRef rightclickRef onMove = do
  log "touchmove"
  preventDefault $ TouchEvent.toEvent te
  if numTouches te > 1
    then pure unit
    else do
      Ref.write false clickRef
      Ref.write false rightclickRef
      let touches = TouchEvent.touches te
      case map getTouchXY $ extractFirstTouch touches of
        Just xy → do log $ "handleMouseMove " <> show xy
                     onMove xy
        Nothing → pure unit


-- To provide Int,Int to event, need to get coords of changedTouch
handleTouchEnd ∷ TouchEvent.TouchEvent → Ref Boolean → Ref Boolean → (Tuple Int Int → Effect Unit) → Effect Unit
handleTouchEnd te clickRef rightclickRef onClick = do
  log "touchend"
  if numTouches te > 1
    then pure unit
    else do
      Ref.write false rightclickRef
      click ← Ref.read clickRef
      if click
        then do
          let touches = TouchEvent.changedTouches te
          case map getTouchXY $ extractFirstTouch touches of
            Just xy → do log "click"
                         onClick xy
            Nothing → pure unit
        else pure unit


numTouches ∷ TouchEvent.TouchEvent → Int
numTouches =
  TouchList.length <<< TouchEvent.touches

extractFirstTouch ∷ TouchList.TouchList → Maybe Touch.Touch
extractFirstTouch =
  TouchList.item 0


getTouchXY ∷ Touch.Touch → Tuple Int Int
getTouchXY t =
  Tuple (Touch.clientX t) (Touch.clientY t)
