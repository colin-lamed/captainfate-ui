module Motor.Model.Common where

import Prelude
import Control.Monad.State  (get, gets, modify_)
import Data.Array as A
import Data.Int (floor, toNumber)
import Data.Lens ((%~), (.~), (^.), (%=), (.=))
import Data.Lens.At (at)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Random (randomInt)
import Effect.Unsafe           (unsafePerformEffect)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Motor.Model.Geometry as G
import Motor.Model.Lens
import Motor.Model.Types
import Motor.Util (arrange)
import Partial.Unsafe (unsafePartial)
import Data.Time.Duration (Milliseconds(..))


getPrep ∷ Verb → Object → Maybe String
getPrep v o =
  toPrep $ M.lookup v $ o ^. oInterceptors
  where
    toPrep (Just (ObjRoutine2 {prep})) = Just prep
    toPrep _                           = Nothing

applyRoutine ∷ ObjRoutine → Maybe Oid → SW Unit
applyRoutine (ObjRoutine1 {routine1}) _         = routine1
applyRoutine (ObjRoutine2 {routine2}) (Just o2) = routine2 o2
applyRoutine (ObjRoutine2 _         ) Nothing   = pure unit



applyVerb ∷ Verb → Oid → Maybe Oid → SW Unit
applyVerb v oid1 moid2 = do
  o1 ← toObj oid1
  fromMaybe (pure mempty) $ map (flip applyRoutine $ moid2) $ M.lookup v $ o1 ^. oInterceptors
  wActionCountMs .= Just (Milliseconds 500.0)

clearSelection ∷ SW Unit
clearSelection = do
  get >>= pure <<< (_ ^. wStory <<< sDefaultVerb) >>= selectVerb
  wSelectedObject .= Nothing

-- | Get the current state value.
getState ∷ ∀ a. Sid a → SW a
getState sid = do
  mf ← gets (_ ^. wStory <<< sStates <<< at (unwrap sid))
  pure $ unsafeFromForeign $ unsafePartial fromJust mf

-- | Set the state value.
setState ∷ ∀ a. Sid a → a → SW Unit
setState sid val =
  (wStory <<< sStates) %= (M.insert (unwrap sid) $ unsafeToForeign val)

say ∷ Aid → String → SW Unit
say aid' say =
  sayAndThen aid' say (pure unit)

-- TODO use continuations?
sayAndThen ∷ Aid → String → SW Unit → SW Unit
sayAndThen aid' say andThen = do
  color ← toActor aid' >>= pure <<< (_ ^. aColor)
  wDisplayText .= Just (DisplayText { text : say
                                    , ms   : Milliseconds 1500.0
                                    , color
                                    , andThen
                                    }
                        )

after ∷ Int → SW Unit → SW Unit
after timeout andThen =
  wNewTimeouts %= A.cons { timeout : Milliseconds $ toNumber timeout
                         , andThen
                         }

selectVerb ∷ Verb → SW Unit
selectVerb verb =
  wSelectedVerb .= Just verb

toRoom ∷ Rid → SW Room
toRoom rid' =
  get >>= pure <<< unsafePartial fromJust <<< (_ ^. wStory <<< sRooms <<< at rid')

toObj ∷ Oid → SW Object
toObj oid' =
  get >>= pure <<< unsafePartial fromJust <<< (_ ^. wStory <<< sObjects <<< at oid')



toActor ∷ Aid → SW Actor
toActor aid' =
  get >>= pure <<< unsafePartial fromJust <<< (_ ^. wStory <<< sActors <<< at aid')

oidInRoom ∷ Rid → SW (Array Oid)
oidInRoom rid' = (<>) <$> map (map fst <<< (_ ^. rHotspots)) r
                      <*> map (map fst <<< (_ ^. rItems)) r
  where r = toRoom rid'

objInRoom ∷ Rid → SW (Array Object)
objInRoom rid' =
  oidInRoom rid' >>= traverse toObj

moveTo ∷ Rid → SW Unit
moveTo newRoom = do
  previousRoom ← getCurrentRoom
  toRoom previousRoom >>= (_ ^. rOnExit)
  -- give objects a chance to respond to exit too (what about inventory?)
  objInRoom previousRoom >>= traverse_ (_ ^. oOnExit)
  modify_ $   (wStory <<< sCurrentRoom .~ newRoom)
          <<< (wMouseOverObject .~ Nothing)
          <<< (wDisplayText .~ Nothing)


                  -- also reset stuff (TODO should be done in enter room hook?)
                  --             action ∷ Boolean,
                  --               actionCountMs ∷ Int,
  -- give objects a chance to respond to enter too
  objInRoom newRoom >>= traverse_ (_ ^. oOnEnter)
  toRoom newRoom >>= (_ ^. rOnEnter)
  modifyRoom newRoom $ rVisitCount %~ (_ + 1)

modifyRoom ∷ Rid → (Room → Room) → SW Unit
modifyRoom ref f =
  wStory <<< sRooms %= M.update (Just <<< f) ref

modifyObject ∷ Oid → (Object → Object) → SW Unit
modifyObject ref f =
  wStory <<< sObjects %= M.update (Just <<< f) ref

addToInventory ∷ Oid → SW Unit
addToInventory oid' = do
  currentRoom ← getCurrentRoom
  modifyRoom currentRoom $ rItems %~ A.filter (\(Tuple o' _) → o' /= oid')
  wStory <<< sInventory %= A.cons oid'

addUniqueToInventory ∷ Oid → SW Unit
addUniqueToInventory oid' = do
  has ← hasInInventory oid'
  if has then pure unit else addToInventory oid'

removeFromInventory ∷ Oid → SW Unit
removeFromInventory oid' =
  wStory <<< sInventory %= A.filter (_ /= oid')

hasInInventory ∷ Oid → SW Boolean
hasInInventory oid' =
  get >>= pure <<< A.elem oid' <<< (_ ^. wStory <<< sInventory)

removeFromRoom ∷ Oid → SW Unit
removeFromRoom o = do
  rooms ← get >>= pure <<< M.values <<< (_ ^. wStory <<< sRooms)
  case A.find (A.elem o <<< map fst <<< (_ ^. rItems)) rooms of
    Just room' → modifyRoom (room' ^. rid) $ rItems %~ A.filter (\(Tuple o' _) → o'/= o)
    Nothing  → pure unit

addToCurrentRoom ∷ Oid → G.Point → SW Unit
addToCurrentRoom o loc = do
  currentRoom ← getCurrentRoom
  modifyRoom currentRoom $ rItems %~ A.cons (Tuple o loc)

inRoom ∷ Rid → Oid → SW Boolean
inRoom rid' oid' = do
  room' ← toRoom rid'
  pure $ oid' `A.elem` (map (\(Tuple o _) → o) $ room' ^. rItems)

addHotspot ∷ Oid → G.Poly → SW Unit
addHotspot o poly = do
  currentRoom ← getCurrentRoom
  modifyRoom currentRoom $ rHotspots %~ A.cons (Tuple o poly)

setGameOver ∷ SW Unit
setGameOver =
  wGameOver .= true

talkOptions ∷ Array { say ∷ String, andThen ∷ SW Unit } → SW Unit
talkOptions tos =
  wTalkOptions .= Just tos

talkOption ∷ String → SW Unit → { say ∷ String, andThen ∷ SW Unit }
talkOption say andThen = { say, andThen }

restoreVerbs ∷ SW Unit
restoreVerbs =
  wTalkOptions .= Nothing

clearTextOption ∷ SW Unit
clearTextOption =
  wTalkOptions .= Just []

withRandomInt ∷ Int → Int → SW Int
withRandomInt min max =
  pure $ unsafePerformEffect $ randomInt min max

takeAtRandom ∷ ∀ a. Array a → SW a
takeAtRandom as = do
  n ← withRandomInt 0 (A.length as - 1)
  pure $ unsafePartial fromJust $ as A.!! n


getBitmap ∷ Graphics → Image → Bitmap
getBitmap g i
  = unsafePartial fromJust $ map snd $ A.find (\(Tuple i' _) → i' == i) g

getCurrentRoom ∷ SW Rid
getCurrentRoom =
  get >>= pure <<< (_ ^. wStory <<< sCurrentRoom)

verbScreenPositions ∷ World → Array (Tuple Verb G.Point)
verbScreenPositions w =
    arrange 3 toPos $ w ^. (wStory <<< sVerbs)
  where
    toPos verb x y = Tuple verb (Tuple (xPos x) (yPos y))
    xPos x = 30.0 + 150.0 * toNumber x
    yPos y = 600.0 + 50.0 * toNumber y

inventoryScreenPositions ∷ World → Array (Tuple Oid G.Point)
inventoryScreenPositions w =
    arrange numCols toPos visibleInventory
  where
    numCols = 4
    row = w ^. wInventoryRow
    visibleInventory = A.take (2 * numCols) $ A.drop (row * numCols) $ w ^. wStory <<< sInventory
    toPos o x y = Tuple o (Tuple (xPos x) (yPos y))
    xPos x = 520.0 + 170.0 * toNumber x
    yPos y = 520.0 + 100.0 * toNumber y

textOptionScreenPositions ∷ World → Array (Tuple { say ∷ String, andThen ∷ SW Unit } G.Point)
textOptionScreenPositions w =
  case w ^. wTalkOptions of
    Just tos → A.zipWith f tos (A.range 0 100) -- 100 is larger than we need
    Nothing  → []
  where
    f to i = Tuple to (Tuple 100.0 (560.0 + 60.0 * toNumber i))

getItems ∷ SW (Array (Tuple Object G.Point))
getItems =
  getCurrentRoom >>= toRoom >>= pure <<< (_ ^. rItems) >>= mapTuple toObj

mapTuple ∷ ∀ a b c. (a → SW b) → Array (Tuple a c) → SW (Array (Tuple b c))
mapTuple g =
  traverse (\(Tuple a c) → map (\b → (Tuple b c)) (g a))

canInvUp∷ World → Boolean
canInvUp = (_ /= 0) <<< (_ ^. wInventoryRow)

canInvDown ∷ World → Boolean
canInvDown w =
  let invCols = 4.0
      numInv = A.length $ w ^. wStory <<< sInventory
  in (w ^. wInventoryRow) /= (floor $ (toNumber numInv) / invCols)


invArrUpPos ∷ G.Point
invArrUpPos = Tuple 500.0 550.0

invArrDownPos ∷ G.Point
invArrDownPos = Tuple 500.0 650.0
