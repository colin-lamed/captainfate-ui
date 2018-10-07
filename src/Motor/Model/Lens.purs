module Motor.Model.Lens where


import Prelude (Unit)
import Data.Lens (Lens', lens)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Newtype as NT
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple)
import Foreign (Foreign)
import Motor.Model.Geometry (Point, Poly)
import Motor.Model.Types


---------------

rid ∷ Lens' Room Rid
rid = lens (\r → (unwrap r).rid) (\r rid' → NT.over Room _ { rid = rid' } r)

rImage ∷ Lens' Room (SW Image)
rImage = lens (\r → (unwrap r).image) (\r image → NT.over Room _ { image = image } r)

rHotspots ∷ Lens' Room (Array (Tuple Oid Poly))
rHotspots = lens (\r → (unwrap r).hotspots) (\r hotspots → NT.over Room _ { hotspots = hotspots } r)

rItems ∷ Lens' Room (Array (Tuple Oid Point))
rItems = lens (\r → (unwrap r).items) (\r items → NT.over Room _ { items = items } r)

rOnEnter ∷ Lens' Room (SW Unit)
rOnEnter = lens (\r → (unwrap r).onEnter) (\r onEnter → NT.over Room (_ { onEnter = onEnter }) r)

rOnExit ∷ Lens' Room (SW Unit)
rOnExit = lens (\r → (unwrap r).onExit) (\r onExit → NT.over Room _ { onExit = onExit } r)

rVisitCount ∷ Lens' Room Int
rVisitCount = lens (\r → (unwrap r).visitCount) (\r visitCount → NT.over Room _ { visitCount = visitCount } r)

rState ∷ Lens' Room (M.Map String String)
rState = lens (\r → (unwrap r).state) (\r state → NT.over Room _ { state = state } r)

---------------

oid ∷ Lens' Object Oid
oid = lens (\o → (unwrap o).oid) (\o oid' → NT.over Object _ { oid = oid' } o)

oName ∷ Lens' Object String
oName = lens (\o → (unwrap o).name) (\o name → NT.over Object _ { name = name } o)

oInterceptors ∷ Lens' Object (M.Map Verb ObjRoutine)
oInterceptors = lens (\o → (unwrap o).interceptors) (\o interceptors → NT.over Object _ { interceptors = interceptors } o)

oImage ∷ Lens' Object (SW (Maybe Image))
oImage = lens (\o → (unwrap o).image) (\o image → NT.over Object _ { image = image } o)

oInventoryImage ∷ Lens' Object (SW (Maybe Image))
oInventoryImage = lens (\o → (unwrap o).inventoryImage) (\o inventoryImage → NT.over Object _ { inventoryImage = inventoryImage } o)

oSuggestedVerb ∷ Lens' Object (SW Verb)
oSuggestedVerb = lens (\o → (unwrap o).suggestedVerb) (\o suggestedVerb → NT.over Object _ { suggestedVerb = suggestedVerb } o)

oInitial ∷ Lens' Object (SW Unit)
oInitial = lens (\o → (unwrap o).initial) (\o initial → NT.over Object _ { initial = initial } o)

oOnEnter ∷ Lens' Object (SW Unit)
oOnEnter = lens (\o → (unwrap o).onEnter) (\o onEnter → NT.over Object _ { onEnter = onEnter } o)

oOnExit ∷ Lens' Object (SW Unit)
oOnExit = lens (\o → (unwrap o).onExit) (\o onExit → NT.over Object _ { onExit = onExit } o)

---------------

aid ∷ Lens' Actor Aid
aid = lens (\a → (unwrap a).aid) (\a aid' → NT.over Actor _ { aid = aid' } a)

aName ∷ Lens' Actor String
aName = lens (\a → (unwrap a).name) (\a name → NT.over Actor _ { name = name } a)

aColor ∷ Lens' Actor Color
aColor = lens (\a → (unwrap a).color) (\a color → NT.over Actor _ { color = color } a)

-- aImage ∷ Lens' Actor (SW Image)
-- aImage = lens (\a → (unwrap a)._aImage) (\a aImage → NT.over Actor _ { _aImage = aImage } a)


---------------

dtText ∷ Lens' DisplayText String
dtText = lens (\dt → (unwrap dt).text) (\dt text → NT.over DisplayText _ { text = text } dt)

dtMs ∷ Lens' DisplayText Milliseconds
dtMs = lens (\dt → (unwrap dt).ms) (\dt ms → NT.over DisplayText _ { ms = ms } dt)

dtColor ∷ Lens' DisplayText Color
dtColor = lens (\dt → (unwrap dt).color) (\dt color → NT.over DisplayText _ { color = color } dt)

dtAndThen ∷ Lens' DisplayText (SW Unit)
dtAndThen = lens (\dt → (unwrap dt).andThen) (\dt andThen → NT.over DisplayText _ { andThen = andThen } dt)


---------------

sTitle ∷ Lens' Story String
sTitle = lens (\s → (unwrap s).title) (\s title → NT.over Story _ { title = title } s)

sVerbs ∷ Lens' Story (Array Verb)
sVerbs = lens (\s → (unwrap s).verbs) (\s verbs → NT.over Story _ { verbs = verbs } s)

sDefaultVerb ∷ Lens' Story Verb
sDefaultVerb = lens (\s → (unwrap s).defaultVerb) (\s defaultVerb → NT.over Story _ { defaultVerb = defaultVerb } s)

sCurrentRoom ∷ Lens' Story Rid
sCurrentRoom = lens (\s → (unwrap s).currentRoom) (\s currentRoom → NT.over Story _ { currentRoom = currentRoom } s)

sCurrentActor ∷ Lens' Story Aid
sCurrentActor = lens (\s → (unwrap s).currentActor) (\s currentActor → NT.over Story _ { currentActor = currentActor } s)

sRooms ∷ Lens' Story (M.Map Rid Room)
sRooms = lens (\s → (unwrap s).rooms) (\s rooms → NT.over Story _ { rooms = rooms } s)

sObjects ∷ Lens' Story (M.Map Oid Object)
sObjects = lens (\s → (unwrap s).objects) (\s objects → NT.over Story _ { objects = objects } s)

sActors ∷ Lens' Story (M.Map Aid Actor)
sActors = lens (\s → (unwrap s).actors) (\s actors → NT.over Story _ { actors = actors } s)

sImages ∷ Lens' Story (Array Image)
sImages = lens (\s → (unwrap s).images) (\s images → NT.over Story _ { images = images } s)

sInitial ∷ Lens' Story (SW Unit)
sInitial = lens (\s → (unwrap s).initial) (\s initial → NT.over Story _ { initial = initial } s)

sStates ∷ Lens' Story (M.Map String Foreign)
sStates = lens (\s → (unwrap s).states) (\s states → NT.over Story _ { states = states } s)

sInventory ∷ Lens' Story (Array Oid)
sInventory = lens (\s → (unwrap s).inventory) (\s inventory → NT.over Story _ { inventory = inventory } s)

---------------

wStory ∷ Lens' World Story
wStory = lens (\w → (unwrap w).story) (\w story → NT.over World _ { story = story } w)

wGraphics ∷ Lens' World Graphics
wGraphics = lens (\w → (unwrap w).graphics) (\w graphics → NT.over World _ { graphics = graphics } w)

wSelectedVerb ∷ Lens' World (Maybe Verb)
wSelectedVerb = lens (\w → (unwrap w).selectedVerb) (\w selectedVerb → NT.over World _ { selectedVerb = selectedVerb } w)

wMouseOverObject ∷ Lens' World (Maybe Oid)
wMouseOverObject = lens (\w → (unwrap w).mouseOverObject) (\w mouseOverObject → NT.over World _ { mouseOverObject = mouseOverObject } w)

wSelectedObject ∷ Lens' World (Maybe Oid)
wSelectedObject = lens (\w → (unwrap w).selectedObject) (\w selectedObject → NT.over World _ { selectedObject = selectedObject } w)

wMouseOverVerb ∷ Lens' World (Maybe Verb)
wMouseOverVerb = lens (\w → (unwrap w).mouseOverVerb) (\w mouseOverVerb → NT.over World _ { mouseOverVerb = mouseOverVerb } w)

wMouseOverTextOption ∷ Lens' World (Maybe String)
wMouseOverTextOption = lens (\w → (unwrap w).mouseOverTextOption) (\w mouseOverTextOption → NT.over World _ { mouseOverTextOption = mouseOverTextOption } w)

wMouseOverInvArrow ∷ Lens' World (Maybe InvArrowDir)
wMouseOverInvArrow = lens (\w → (unwrap w).mouseOverInvArrow) (\w mouseOverInvArrow → NT.over World _ { mouseOverInvArrow = mouseOverInvArrow } w)

wDisplayText ∷ Lens' World (Maybe DisplayText)
wDisplayText = lens (\w → (unwrap w).displayText) (\w displayText → NT.over World _ { displayText = displayText } w)

wGameOver ∷ Lens' World Boolean
wGameOver = lens (\w → (unwrap w).gameOver) (\w gameOver → NT.over World _ { gameOver = gameOver } w)

wActionCountMs ∷ Lens' World (Maybe Milliseconds)
wActionCountMs = lens (\w → (unwrap w).actionCountMs) (\w actionCountMs → NT.over World _ { actionCountMs = actionCountMs } w)

wTalkOptions ∷ Lens' World (Maybe (Array { say :: String, andThen :: SW Unit }))
wTalkOptions = lens (\w → (unwrap w).talkOptions) (\w talkOptions → NT.over World _ { talkOptions = talkOptions } w)

wInventoryRow ∷ Lens' World Int
wInventoryRow = lens (\w → (unwrap w).inventoryRow) (\w inventoryRow → NT.over World _ { inventoryRow = inventoryRow } w)

wTimeouts ∷ Lens' World (Array { timeout ∷ Milliseconds, andThen ∷ SW Unit })
wTimeouts = lens (\w → (unwrap w).timeouts) (\w timeouts → NT.over World _ { timeouts = timeouts } w)

wNewTimeouts ∷ Lens' World (Array { timeout ∷ Milliseconds, andThen ∷ SW Unit })
wNewTimeouts = lens (\w → (unwrap w).newTimeouts) (\w newTimeouts → NT.over World _ { newTimeouts = newTimeouts } w)

wCanvasDim ∷ Lens' World { width :: Number, height :: Number }
wCanvasDim = lens (\w → (unwrap w).canvasDim) (\w canvasDim → NT.over World _ { canvasDim = canvasDim } w)

wCursorPos ∷ Lens' World (Tuple Int Int)
wCursorPos = lens (\w → (unwrap w).cursorPos) (\w cursorPos → NT.over World _ { cursorPos = cursorPos } w)
