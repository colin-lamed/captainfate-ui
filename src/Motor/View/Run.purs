module Motor.View.Run
  ( run
  ) where

import Prelude
import Control.Monad.State   (execState, execStateT)
import Control.Monad.Except.Trans (ExceptT, except, runExceptT)
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..), either)
import Data.Int (toNumber, round)
import Data.Lens ((^.), (.~))
import Data.Maybe (fromJust, maybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, makeAff, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console (error)
import Effect.Exception as Ex
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas as C
import Motor.Model.Core (initialWorld, toStory)
import Motor.Model.Lens (wCanvasDim, sImages, sTitle)
import Motor.Model.Types (Graphics, Story, StoryDef, World)
import Motor.View.Canvas (Canvas)
import Motor.View.Draw (renderWorld, renderMsg, ImgCache, newCache, clearCache)
import Motor.View.Event as E
import Motor.View.Touch as Touch
import Motor.View.Update (stepWorld)
import Motor.View.Util (addHead, innerHeight, innerWidth, hideCursor)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Web.DOM.Document (toEventTarget) as Doc
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (click, blur, focus)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (focus) as Elmt
import Web.HTML.Window (Window, document, requestAnimationFrame)
import Web.HTML.Window (toEventTarget) as Win
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes (mousemove, mouseout)
import Web.TouchEvent.EventTypes (touchstart, touchmove, touchend)
import Web.TouchEvent.TouchEvent (TouchEvent)
import Web.TouchEvent.TouchEvent as TE
import Unsafe.Coerce (unsafeCoerce)


run ∷ StoryDef → Effect Unit
run storyDef = do
  win       ← window
  doc       ← map toDocument $ document win
  c         ← map (unsafePartial fromJust) (C.getCanvasElementById "canvas")
  Tuple w h ← resizeCanvas c
  renderMsg "Loading, please wait..." c w h
  hideCursor doc

  flip runAff_ (run2 storyDef) $ case _ of
    Left err → do error $ show err
                  renderMsg "Error..." c w h
    Right g  → pure unit


eitherToAff :: forall a. Either String a -> Aff a
eitherToAff (Left err) = throwError $ Ex.error err
eitherToAff (Right a)  = pure a

run2 ∷ StoryDef → Aff Unit
run2 storyDef = do
  win       ← liftEffect $ window
  doc       ← liftEffect $ map toDocument $ document win
  c         ← liftEffect $ map (unsafePartial fromJust) (C.getCanvasElementById "canvas")

  s   ← eitherToAff $ lmap ("failed to create story: " <> _) (toStory storyDef)

  liftEffect $ addHead doc $ "<title>" <> s ^. sTitle <> "</title>"

  g ← loadGraphics s

  liftEffect $ do
    worldRef      ← initialWorld s g >>= Ref.new
    stopRef       ← Ref.new false
    lastTimeRef   ← now >>= Ref.new
    clickRef      ← Ref.new false
    rightclickRef ← Ref.new false
    imgCache      ← newCache

    on (Doc.toEventTarget doc) blur        \e → clearCache imgCache *> pause stopRef
    on (Doc.toEventTarget doc) focus       \e → resume stopRef worldRef lastTimeRef imgCache win c g
    on (Win.toEventTarget win) resize      \e → (resizeCanvas c >>= storeCanvasSize worldRef) *> clearCache imgCache
    on (Win.toEventTarget win) touchstart  \e → handleTouchStart clickRef rightclickRef worldRef imgCache          (unsafePartial fromJust $ TE.fromEvent e)
    on (Win.toEventTarget win) touchmove   \e → handleTouchMove  clickRef rightclickRef worldRef                   (unsafePartial fromJust $ TE.fromEvent e)
    on (Win.toEventTarget win) touchend    \e → handleTouchEnd   clickRef rightclickRef worldRef imgCache          (unsafePartial fromJust $ TE.fromEvent e)
    on (unsafeCoerce c)        mousemove   \e → handleMouseMove worldRef                                           (unsafePartial fromJust $ ME.fromEvent e)
    on (unsafeCoerce c)        mouseout    \e → handleMouseMove worldRef                                           (unsafePartial fromJust $ ME.fromEvent e)
    on (unsafeCoerce c)        click       \e →                     handleClick worldRef imgCache                  (unsafePartial fromJust $ ME.fromEvent e)
    on (unsafeCoerce c)        contextmenu \e → preventDefault e *> handleClick worldRef imgCache                  (unsafePartial fromJust $ ME.fromEvent e)

    { width: w, height: h } ← C.getCanvasDimensions c
    storeCanvasSize worldRef (Tuple (round w) (round h))

    Elmt.focus (unsafeCoerce c)
    resume stopRef worldRef lastTimeRef imgCache win c g

    pure unit

on :: forall a. EventTarget -> EventType -> (Event -> Effect a) -> Effect Unit
on eventtarget eventtype f =
  eventListener f >>= \el ->
    addEventListener eventtype el false eventtarget


handleTouchStart ∷ Ref Boolean → Ref Boolean → Ref World → ImgCache → TouchEvent → Effect Unit
handleTouchStart clickRef rightclickRef worldRef imgCache te =
  void $ launchAff $ do
    xy ← Touch.handleTouchStart te clickRef rightclickRef
    liftEffect $ Ref.modify_ (execState $ E.handleRightClick xy) worldRef
    liftEffect $ clearCache imgCache

handleTouchMove ∷ Ref Boolean → Ref Boolean → Ref World → TouchEvent → Effect Unit
handleTouchMove clickRef rightclickRef worldRef te =
  Touch.handleTouchMove te clickRef rightclickRef $ \xy →
    Ref.modify_ (execState $ E.handleMouseMove xy) worldRef

handleTouchEnd ∷ Ref Boolean → Ref Boolean → Ref World → ImgCache → TouchEvent → Effect Unit
handleTouchEnd clickRef rightclickRef worldRef imgCache te =
  Touch.handleTouchEnd te clickRef rightclickRef $ \xy → do
    Ref.modify_ (execState $ E.handleLeftClick xy) worldRef
    clearCache imgCache

handleMouseMove ∷ Ref World → MouseEvent → Effect Unit
handleMouseMove worldRef me = do
  let xy = Tuple (ME.clientX me) (ME.clientY me)-- mouseOffsetXY
  Ref.modify_ (execState $ E.handleMouseMove xy) worldRef

handleClick ∷ Ref World → ImgCache → MouseEvent → Effect Unit
handleClick worldRef imgCache me = do
  let xy = Tuple (ME.clientX me) (ME.clientY me)-- mouseOffsetXY
  Ref.modify_ (execState $ case ME.button me of
                  0 → E.handleLeftClick xy
                  2 → E.handleRightClick xy
                  _ → pure unit
              ) worldRef
  clearCache imgCache

resizeCanvas ∷ Canvas → Effect (Tuple Int Int)
resizeCanvas c = do
  w ← innerWidth
  h ← innerHeight
  C.setCanvasDimensions c { width: toNumber w, height: toNumber h }
  pure (Tuple w h)

storeCanvasSize ∷ Ref World → Tuple Int Int → Effect Unit
storeCanvasSize worldRef (Tuple w h) = do
  let width  = 0.95 * (toNumber w)
      height = 0.95 * (toNumber h)
  Ref.modify_ (wCanvasDim .~ {width, height}) worldRef

pause ∷ Ref Boolean → Effect Unit
pause =
  Ref.write true

resume ∷ Ref Boolean → Ref World → Ref Instant → ImgCache → Window → Canvas → Graphics → Effect Unit
resume stopRef worldRef lastTimeRef imgCache win c g = do
  Ref.write false stopRef
  let callback = step stopRef worldRef lastTimeRef imgCache win c g
  void $ requestAnimationFrame callback win

step ∷ Ref Boolean → Ref World → Ref Instant → ImgCache → Window → Canvas → Graphics → Effect Unit
step stopRef worldRef lastTimeRef imgCache win c g = do
  t ← now
  lastT ← Ref.read lastTimeRef
  Ref.write t lastTimeRef
  -- limit to 1s if was backgrounded
  let dt = Milliseconds $ min 1000.0 (unwrap (unInstant t) - unwrap (unInstant lastT))
  w ← Ref.read worldRef
  w' ← execStateT (stepWorld dt imgCache) w
  Ref.write w' worldRef
  renderWorld imgCache c g w'

  unlessM (Ref.read stopRef) $
    resume stopRef worldRef lastTimeRef imgCache win c g


loadGraphics ∷ Story → Aff Graphics
loadGraphics s = do
  for (s ^. sImages) \img →
    makeAff \callback →
      do C.tryLoadImage (unwrap img).filepath $
            callback <<< maybe (Left $ Ex.error $ "Could not load " <> (unwrap img).filepath)
                               (Right <<< Tuple img)
         pure mempty

resize ∷ EventType
resize = EventType "resize"

contextmenu ∷ EventType
contextmenu = EventType "contextmenu"
