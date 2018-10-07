module Motor.View.Draw
  ( ImgCache
  , newCache
  , clearCache
  , renderMsg
  , renderWorld
  , white
  , scale
  ) where

import Prelude
import Control.Monad.State      (get, gets, evalState)
import Data.Bifunctor (bimap)
import Data.Int (toNumber, round)
import Data.Lens                    ((^.))
import Data.Map                       as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_, sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (Context2D, getContext2D)
import Math as Math
import Motor.Model.Common (verbScreenPositions, getItems,
  inventoryScreenPositions, textOptionScreenPositions, getBitmap, toObj, getPrep,
  toRoom, getCurrentRoom, invArrUpPos, invArrDownPos, canInvUp, canInvDown)
import Motor.Model.Geometry    as G
import Motor.Model.Lens
import Motor.Model.Types hiding (Bitmap, Color)
import Motor.View.Canvas (Bitmap, Canvas, Color(..), Picture)
import Motor.View.Canvas as C


-- TOOD fine grained cache - i.e. invalidate parts not all of it
newtype ImgCache = ImgCache { ref ∷ Ref (Map.Map String Bitmap) }

derive instance newtypeImgCache ∷ Newtype ImgCache _

newCache ∷ Effect ImgCache
newCache = Ref.new Map.empty >>= \ref → pure $ ImgCache { ref }

clearCache ∷ ImgCache → Effect Unit
clearCache imgCache =
  Ref.write Map.empty (unwrap imgCache).ref

getCache ∷ ∀ a. String → Ref (Map.Map String a) → Effect a → Effect a
getCache cacheKey cacheRef calc = do
  cache ← Ref.read cacheRef
  case Map.lookup cacheKey cache of
    Just v  → pure v
    Nothing → do
      v ← calc
      Ref.modify_ (Map.insert cacheKey v) cacheRef
      pure v

white ∷ Color
white  = RGB 255 255 255

orange ∷ Color
orange = RGB 255 165 0

black ∷ Color
black  = RGB 0   0   0

grey ∷ Color
grey = RGB 64 64 64

-- scale to canvas, where picture  is assuming size 800x1200
-- TODO verb area should have max size, then scale image to fill the rest? (else can be too small on some devices to click?)
scale :: World → Number
scale w =
  let canvasDim = w ^. wCanvasDim
  in min (canvasDim.width / 1200.0) (canvasDim.height / 800.0)

renderMsg ∷ String → Canvas → Int → Int → Effect Unit
renderMsg msg c w h = do
  let s = min (toNumber w / 1200.0) (toNumber h / 800.0)
  C.render c $ C.scale (Tuple s s) $ do
    C.color black $ C.fill $ C.rect (Tuple 0.0 0.0) (Tuple 1200.0 800.0)
    C.color white $ C.translate (Tuple 400.0 400.0) $ C.scale (Tuple 3.0 3.0) $ C.text (Tuple 0.0 0.0) msg

render' ∷ String → ImgCache → Canvas → Number → Picture Unit → Effect Unit
render' cacheKey imgCache c s calc =  do
  bm ← getCache cacheKey (unwrap imgCache).ref $ do
    C.buffer (round $ s * 1200.0) (round $ s * 800.0) $ calc
  C.render c $ C.draw bm (Tuple 0.0 0.0)

renderOnTop' ∷ String → ImgCache → Context2D → Number → Picture Unit → Effect Unit
renderOnTop' cacheKey imgCache ctx s calc =  do
  bm ← getCache cacheKey (unwrap imgCache).ref $ do
    C.buffer (round $ s * 1200.0) (round $ s * 800.0) $ calc
  C.renderOnTop ctx $ C.draw bm (Tuple 0.0 0.0)

-- TODO can we reuse graphics instead of another image cache for prerendered images?
renderWorld ∷ ImgCache → Canvas → Graphics → World → Effect Unit
renderWorld imgCache c g w = do
  ctx ← getContext2D c
  let s = scale w

  render' "room" imgCache c s $ C.scale (Tuple s s) $ do
    drawRoom g w
    drawVerbArea

  renderOnTop' "text" imgCache ctx s $ C.scale (Tuple s s) $ drawText w

  if w ^. wGameOver
    then
      C.renderOnTop ctx $ C.scale (Tuple s s) $ drawGameOver
    else case w ^. wTalkOptions of
      Just _  → C.renderOnTop ctx $ C.scale (Tuple s s) $ drawTextOptions w
      Nothing → do
        renderOnTop' "inv" imgCache ctx s $ C.scale (Tuple s s) $ drawInventory g w
        C.renderOnTop ctx $ C.scale (Tuple s s) $ drawInvArrows w
        C.renderOnTop ctx $ C.scale (Tuple s s) $ drawVerbs w
        C.renderOnTop ctx $ C.scale (Tuple s s) $ drawActionText w
  C.renderOnTop ctx $ drawCursor w


drawPicture ∷ G.Point → Maybe Bitmap → Picture Unit
drawPicture pos (Just bm) = C.draw bm pos
drawPicture _   Nothing   = pure unit

drawRoom ∷ Graphics → World → Picture Unit
drawRoom g w = do
  let roomImage = evalState (getCurrentRoom >>= toRoom >>= (_ ^. rImage) >>= pure <<< getBitmap g) w
      toPic (Tuple o (Tuple x y)) = o ^. oImage >>= pure <<< drawPicture (Tuple (Math.round x) (Math.round y)) <<< map (getBitmap g)
      itemPictures = evalState (getItems >>= traverse toPic) w
  C.draw roomImage (Tuple 0.0 0.0)
  sequence_ itemPictures

drawVerbArea ∷ Picture Unit
drawVerbArea = do
  C.color black $ C.fill   $ C.rect (Tuple 0.0 500.0) (Tuple 1200.0 800.0)
  C.color white $ C.stroke $ C.rect (Tuple 0.0 500.0) (Tuple 1200.0 (800.0 - 1.0))

drawText ∷ World → Picture Unit
drawText w
  = drawText' $ w ^. wDisplayText
  where
    drawText' (Just (DisplayText dt))
      | dt.ms > Milliseconds 0.0 = do C.color black $ C.fill $ C.rect (Tuple 0.0 100.0) (Tuple 1200.0 140.0)
                                      C.translate (Tuple 10.0 130.0) $ C.scale (Tuple 3.0 3.0) $ C.color dt.color $ C.text (Tuple 0.0 0.0) dt.text
    drawText' _ = pure unit

drawVerbs ∷ World → Picture Unit
drawVerbs w = traverse_ drawVerb $ verbScreenPositions w
  where drawVerb (Tuple v pos) = C.color (vColor v) $ C.translate pos $ C.scale (Tuple 3.0 3.0) $ C.text (Tuple 0.0 0.0) (unwrap v).label
        vColor v = if evalState (isVerbToHighlight v) w then orange else white

drawInventory ∷ Graphics → World → Picture Unit
drawInventory g w = do
  let drawInv (Tuple oRef' pos) = drawPicture pos <<< map (getBitmap g) $ img oRef'
      img oRef' = evalState (toObj oRef' >>= (_ ^. oInventoryImage)) w
  traverse_ drawInv $ inventoryScreenPositions w

drawInvArrows ∷ World → Picture Unit
drawInvArrows w = do
  let (Tuple moUp moDown) = case w ^. wMouseOverInvArrow of
        Nothing           → Tuple false false
        Just InvArrowUp   → Tuple true  false
        Just InvArrowDown → Tuple false true
  C.translate invArrUpPos   $ drawInvArrowUp   moUp   $ (canInvUp   w)
  C.translate invArrDownPos $ drawInvArrowDown moDown $ (canInvDown w)

drawInvArrowDown ∷ Boolean → Boolean → Picture Unit
drawInvArrowDown mouseOver active =
  let c = if active then (if mouseOver then orange else white) else grey
  in C.color c $ C.stroke $ C.path
    [ Tuple (-20.0)    0.0
    , Tuple    0.0    20.0
    , Tuple   20.0     0.0
    , Tuple   10.0     0.0
    , Tuple   10.0  (-30.0)
    , Tuple (-10.0) (-30.0)
    , Tuple (-10.0)    0.0
    , Tuple (-20.0)    0.0
    ]

drawInvArrowUp ∷ Boolean → Boolean → Picture Unit
drawInvArrowUp mo = C.rotate Math.pi <<< drawInvArrowDown mo


drawActionText ∷ World → Picture Unit
drawActionText w =
  C.color tColor $ C.translate (Tuple 100.0 540.0) $ C.scale (Tuple 3.0 3.0) $ C.text (Tuple 0.0 0.0) t
  where
    t       = evalState composeActionText w
    tColor  = evalState colorSW w
    colorSW = gets (_ ^. wActionCountMs) >>= \a → pure $ if isJust a then orange else white

drawGameOver ∷ Picture Unit
drawGameOver =
  C.translate (Tuple 300.0 700.0) $ C.scale (Tuple 10.0 10.0) $ C.color white $ C.text (Tuple 0.0 0.0) "The end"


drawTextOptions ∷ World → Picture Unit
drawTextOptions w = traverse_ drawTextOption $ textOptionScreenPositions w
  where drawTextOption (Tuple { say } pos) = C.translate pos $ C.scale (Tuple 3.0 3.0) $ C.color (textColor say) $ C.text (Tuple 0.0 0.0) say
        textColor say = if w ^. wMouseOverTextOption == Just say then orange else white

drawCursor ∷ World → Picture Unit
drawCursor w = do
  let xy = bimap toNumber toNumber $ w ^. wCursorPos
      size = 20.0
  C.color white $ C.translate xy $ C.stroke $ do
    C.line (Tuple (-size)  0.0     ) (Tuple (-3.0)   0.0 )
    C.line (Tuple 3.0      0.0     ) (Tuple size     0.0 )
    C.line (Tuple 0.0      (- size)) (Tuple 0.0    (-3.0))
    C.line (Tuple 0.0      3.0     ) (Tuple 0.0      size)
  -- TODO colour should blink so can identify it (requires time in world)
  --      for now, add black border too (so can always be seen)
  C.color black $ C.translate xy $ C.stroke $ do
    C.path [ Tuple (-size - 2.0) (      - 2.0)
           , Tuple (-size - 2.0) (        2.0)
           , Tuple (      - 2.0) (        2.0)
           , Tuple (      - 2.0) ( size + 2.0)
           , Tuple (        2.0) ( size + 2.0)
           , Tuple (        2.0) (        2.0)
           , Tuple ( size + 2.0) (        2.0)
           , Tuple ( size + 2.0) (      - 2.0)
           , Tuple (        2.0) (      - 2.0)
           , Tuple (        2.0) (-size - 2.0)
           , Tuple (      - 2.0) (-size - 2.0)
           , Tuple (      - 2.0) (      - 2.0)
           , Tuple (-size - 2.0) (      - 2.0)
           ]


--------------------------------------------------------------------------------

isVerbToHighlight ∷ Verb → SW Boolean
isVerbToHighlight verb = do
  isMouseOverVerb  ← gets $ (_ == Just verb) <<< (_ ^. wMouseOverVerb)
  isSuggestedVerb' ← gets (_ ^. wMouseOverObject) >>= isSuggestedVerb verb
  pure $ isMouseOverVerb || isSuggestedVerb'

isSuggestedVerb ∷ Verb → Maybe Oid → SW Boolean
isSuggestedVerb verb (Just oid') = toObj oid' >>= (_ ^. oSuggestedVerb) >>= pure <<< (_ == verb)
isSuggestedVerb _    Nothing     = pure false

composeActionText ∷ SW String
composeActionText = do
  w ← get
  case {a :w ^. wSelectedVerb, b: w ^. wSelectedObject, c: w ^. wMouseOverObject} of
    {a: Nothing, b: _        , c: _          } → pure $ composeActionText' Nothing Nothing Nothing Nothing
    {a: Just v , b: Just oid', c: Just oid2  }
      | oid2 /= oid'                           → do
                                                   o ← toObj oid'
                                                   o2 ← toObj oid2
                                                   pure $ composeActionText' (Just v) (Just o) (getPrep v o) (Just o2)
    {a: Just v , b: Just oid', c: _          } → do
                                                   o ← toObj oid'
                                                   pure $ composeActionText' (Just v) (Just o) (getPrep v o) Nothing
    {a: Just v, b: Nothing   , c:  Nothing   } → pure $ composeActionText' (Just v) Nothing Nothing Nothing
    {a: Just v, b: Nothing   , c:  Just oid' } → do
                                                   o ← toObj oid'
                                                   pure $ composeActionText' (Just v) (Just o) Nothing Nothing

composeActionText' ∷ Maybe Verb → Maybe Object → Maybe String → Maybe Object → String
composeActionText' (Just (Verb {label})) (Just o) (Just p) (Just o2) = label <> " " <> o ^. oName <> " " <> p <> " " <> o2 ^. oName
composeActionText' (Just (Verb {label})) (Just o) (Just p) _         = label <> " " <> o ^. oName <> " " <> p
composeActionText' (Just (Verb {label})) (Just o) _        _         = label <> " " <> o ^. oName
composeActionText' (Just (Verb {label})) _        _        _         = label
composeActionText' _                     _        _        _         = ""
