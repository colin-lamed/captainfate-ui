module Motor.View.Event
  ( handleMouseMove
  , handleLeftClick
  , handleRightClick
  ) where


import Prelude
import Control.Monad.State (get, modify_, evalState)
import Data.Array as A
import Data.Int (toNumber)
import Data.Lens ((.~), (^.), (.=), (%=))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Tuple (Tuple(..), fst)
import Effect.Unsafe (unsafePerformEffect)
import Motor.Model.Common (verbScreenPositions, getItems,
  inventoryScreenPositions, textOptionScreenPositions, selectVerb, clearSelection,
  toObj, toRoom, getPrep, applyVerb, clearTextOption, sayAndThen, getCurrentRoom,
  getBitmap, invArrUpPos, invArrDownPos, canInvUp, canInvDown)
import Motor.Model.Geometry as G
import Motor.Model.Lens
import Motor.Model.Types
import Motor.Util (ifNot)
import Motor.View.Canvas as C
import Motor.View.Draw (scale)

canvasToWorld ∷ World → Tuple Int Int → G.Point
canvasToWorld w (Tuple x y) =
  Tuple (toNumber x / scale w) (toNumber y / scale w)

handleMouseMove ∷ Tuple Int Int → SW Unit
handleMouseMove (Tuple x y) = do
  w ← get
  modify_ $ wCursorPos .~ Tuple x y
  let pos  = canvasToWorld w (Tuple x y)
      o    = w ^. wSelectedVerb *> evalState (getObject pos) w
      mov  = getVerb w pos
      moto = getTextOption w pos >>= \{ say } -> pure say
      moia = getInvArrow pos
  modify_ $   (wMouseOverObject .~ o)
          <<< (wMouseOverVerb .~ mov)
          <<< (wMouseOverTextOption .~ moto)
          <<< (wMouseOverInvArrow .~ moia)

handleLeftClick ∷ Tuple Int Int → SW Unit
handleLeftClick (Tuple x y) = do
  handleMouseMove (Tuple x y)
  w ← get
  let pos = canvasToWorld w (w ^. wCursorPos)
  case getTextOption w pos of
    Just textOption → do
      aid' ← get >>= pure <<< (_ ^. wStory <<< sCurrentActor)
      applyTextOption aid' textOption
    Nothing
      | isJust (w ^. wTalkOptions) → pure unit -- disable walking etc when in talk mode
      | otherwise → case getInvArrow pos of
                      Just InvArrowUp   → when (canInvUp w) $ wInventoryRow %= (\i → i - 1)
                      Just InvArrowDown → when (canInvDown w) $ wInventoryRow %= (_ + 1)
                      Nothing           → case getVerb w pos of
                                            Just v  → clearSelection *> selectVerb v
                                            Nothing → selectObject

handleRightClick ∷ Tuple Int Int → SW Unit
handleRightClick (Tuple x y) = do
  handleMouseMove (Tuple x y)
  w ← get
  case w ^. wMouseOverObject of
    Just oRef' → applyRightButtonOn oRef'
    Nothing    → clearSelection


selectObject ∷ SW Unit
selectObject = do
  world ← get
  case {a: world ^. wMouseOverObject, b: world ^. wSelectedObject, c: world ^. wSelectedVerb} of
    {a: Just oid', b: Nothing  , c: Just v } → do o ← toObj oid'
                                                  case getPrep v o of
                                                    Just _  → -- promote mouseOverObject to selectedObject
                                                               -- we'll apply the verb when we get the second object
                                                               wSelectedObject .= Just oid'
                                                    Nothing → applyVerb v oid' Nothing
    {a: Just oid2, b: Just oid', c: Just v }  → do o ← toObj oid'
                                                   case getPrep v o of
                                                     Just _  → applyVerb v oid' (Just oid2)
                                                     Nothing → -- shouldn't happen but just in case
                                                               applyVerb v oid' Nothing
    {a: Just _   , b:     _    , c: Nothing} → pure unit
    {a: Nothing  , b:     _    , c: _      } → clearSelection



applyRightButtonOn ∷ Oid → SW Unit
applyRightButtonOn oid' = do
  v ← toObj oid' >>= (_ ^. oSuggestedVerb)
  selectVerb v
  get >>= pure <<< (_ ^. wMouseOverObject) >>= applyVerb v oid'

applyTextOption ∷ Aid → { say ∷ String, andThen ∷ SW Unit } → SW Unit
applyTextOption aid' { say, andThen } =
  clearTextOption *> sayAndThen aid' say andThen

getVerb ∷ World → G.Point → Maybe Verb
getVerb world xy =
    map fst $ A.find f $ verbScreenPositions world
  where
    f (Tuple _ (Tuple vx vy)) = G.pointInPoly (G.rect (Tuple (vx - 10.0) (vy - 40.0)) 110 50) xy


getInvArrow ∷ G.Point → Maybe InvArrowDir
getInvArrow xy =
  let Tuple ux uy = invArrUpPos
      Tuple dx dy = invArrDownPos
  in if G.pointInPoly (G.rect (Tuple (ux - 20.0) (uy - 30.0)) 40 60) xy
     then Just InvArrowUp
     else if G.pointInPoly (G.rect (Tuple (dx - 20.0) (dy - 30.0)) 40 60) xy
       then Just InvArrowDown
       else Nothing


getObject ∷ G.Point → SW (Maybe Oid)
getObject sPos =
    ifNot <$> isItem <*> (ifNot <$> isHotspot <*> isInventory)
  where
    isHotspot   = getCurrentRoom >>= toRoom >>= pure <<< onHotspot <<< (_ ^. rHotspots)
    onHotspot   ∷ Array (Tuple Oid G.Poly) → Maybe Oid
    onHotspot   = map fst <<< A.find (\(Tuple _ poly) → G.pointInPoly poly sPos)
    isItem      = getItems >>= map (map ((_ ^. oid) <<< fst)) <<< map L.head <<< L.filterM (isPointOverObj sPos) <<< L.fromFoldable
    isInventory = get >>= pure <<< map fst <<< A.find onInventory <<< inventoryScreenPositions
    onInventory (Tuple _ pos) = G.pointInPoly (G.rect pos 150 100) sPos

isPointOverObj ∷ G.Point → Tuple Object G.Point → SW Boolean
isPointOverObj sPos (Tuple item iPos) = do
  mi ← item ^. oImage
  bm ← get >>= pure <<< (_ ^. wGraphics) >>= \g → pure $ map (getBitmap g) mi
  let itemPoly' = fromMaybe [] $ map (itemPoly iPos) bm
  pure $ maybe false (\_ → G.pointInPoly itemPoly' sPos) mi

itemPoly ∷ G.Point → C.Bitmap → G.Poly
itemPoly (Tuple ix iy) bm = res
  where (Tuple w h) = unsafePerformEffect $ C.bitmapDim bm
        res         = G.rect (Tuple ix iy) w h

getTextOption ∷ World → G.Point → Maybe { say ∷ String, andThen ∷ SW Unit }
getTextOption world (Tuple _ y) =
    map fst $ A.find f $ textOptionScreenPositions world
  where
    f (Tuple _ (Tuple _ vy)) = y >= vy - 40.0 && y <= vy + 10.0 -- since don't know text length - use whole frame width
