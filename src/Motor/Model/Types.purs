module Motor.Model.Types where

import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.State (State)
import Data.Exists (Exists, mkExists, runExists)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple)
import Foreign (Foreign)
import Motor.Model.Geometry (Point, Poly)
import Motor.View.Canvas as C

-- TODO can we catch that all Oids, Rids have been assigned with mkObject, mkRoom early?
--      buildStory may be the earliest place
--      can we check that all image filepaths exist in compilation time?


type Color = C.Color
type Bitmap = C.Bitmap
type FilePath = String

newtype Image = Image { filepath ∷ FilePath}

derive instance eqImage ∷ Eq Image

instance showImage ∷ Show Image where
 show (Image { filepath }) = "Image " <> filepath

derive instance newtypeImage ∷ Newtype Image _


type Graphics = Array (Tuple Image Bitmap)

data InvArrowDir = InvArrowUp | InvArrowDown

instance showInvArrowDir ∷ Show InvArrowDir where
  show InvArrowUp  = "InvArrowUp"
  show InvArrowDown = "InvArrowDown"


newtype Aid   = Aid String

derive instance eqAid ∷ Eq Aid

instance showAid ∷ Show Aid where
  show (Aid s) = "Aid " <> show s

derive instance ordAid ∷ Ord Aid

derive instance newtypeAid ∷ Newtype Aid _

newtype Rid   = Rid String

derive instance eqRid ∷ Eq Rid

instance showRid ∷ Show Rid where
  show (Rid s) = "Rid " <> show s

derive instance ordRid ∷ Ord Rid

derive instance newtypeRid ∷ Newtype Rid _

newtype Oid   = Oid String

derive instance eqOid ∷ Eq Oid

instance showOid ∷ Show Oid where
  show (Oid s) = "Oid " <> show s

derive instance ordOid ∷ Ord Oid

derive instance newtypeOid ∷ Newtype Oid _

newtype Sid a = Sid String

derive instance eqSid ∷ Eq (Sid a)

instance showSid ∷ Show (Sid a) where
  show (Sid s) = "Sid " <> show s

derive instance newtypeSid ∷ Newtype (Sid a) _

newtype Verb = Verb { label ∷ String }

derive instance eqVerb ∷ Eq Verb

instance showVerb ∷ Show Verb where
  show (Verb s) = "Verb " <> show s

derive instance newtypeVerb ∷ Newtype Verb _

derive instance ordVerb ∷ Ord Verb

data ObjRoutine
  = ObjRoutine1 {                routine1 ∷       SW Unit }
  | ObjRoutine2 { prep ∷ String, routine2 ∷ Oid → SW Unit }

newtype Object = Object
  { oid            ∷ Oid
  , name           ∷ String
  , interceptors   ∷ M.Map Verb ObjRoutine
  , image          ∷ SW (Maybe Image)
  , inventoryImage ∷ SW (Maybe Image)
  , suggestedVerb  ∷ SW Verb
  , initial        ∷ SW Unit
  , onEnter        ∷ SW Unit
  , onExit         ∷ SW Unit
  }

derive instance newtypeObject ∷ Newtype Object _

instance showObject ∷ Show Object where
  show (Object { name }) = "Object {" <> name <> "}"

newtype Room = Room
  { rid        ∷ Rid
  , image      ∷ SW Image
  , hotspots   ∷ Array (Tuple Oid Poly)
  , items      ∷ Array (Tuple Oid Point)
  , onEnter    ∷ SW Unit
  , onExit     ∷ SW Unit
  , visitCount ∷ Int
  , state      ∷ M.Map String String
  }

derive instance newtypeRoom ∷ Newtype Room _

instance showRoom ∷ Show Room where
  show (Room { rid }) = "Room {" <> unwrap rid <> "}"

newtype Actor = Actor
  { aid        ∷ Aid
  , name       ∷ String
  , color      ∷ Color
  -- , image      ∷ SW Image
  }

derive instance newtypeActor ∷ Newtype Actor _

instance showActor ∷ Show Actor where
  show (Actor { aid }) = "Actor {" <> unwrap aid <> "}"


newtype DisplayText = DisplayText
  { text    ∷ String
  , ms      ∷ Milliseconds
  , color   ∷ Color
  , andThen ∷ SW Unit
  }

derive instance newtypeDisplayText ∷ Newtype DisplayText _

newtype Story = Story
  { title        ∷ String
  , verbs        ∷ Array Verb
  , defaultVerb  ∷ Verb
  , currentRoom  ∷ Rid
  , currentActor ∷ Aid
  , rooms        ∷ M.Map Rid Room
  , objects      ∷ M.Map Oid Object
  , actors       ∷ M.Map Aid Actor
  , images       ∷ Array Image
  , initial      ∷ SW Unit -- initialRoom could be set by intital, but we would have to define currentRoom as Maybe Rid
  , states       ∷ M.Map String Foreign
  , inventory    ∷ Array Oid
  }

derive instance newtypeStory ∷ Newtype Story _

newtype World = World
  { story               ∷ Story
  , graphics            ∷ Graphics
  , selectedVerb        ∷ Maybe Verb
  , mouseOverObject     ∷ Maybe Oid
  , selectedObject      ∷ Maybe Oid
  , mouseOverVerb       ∷ Maybe Verb
  , mouseOverTextOption ∷ Maybe String
  , mouseOverInvArrow   ∷ Maybe InvArrowDir
  , displayText         ∷ Maybe DisplayText
  , gameOver            ∷ Boolean
  -- |count down for presenting action as acted before clearing
  , actionCountMs       ∷ Maybe Milliseconds
  , talkOptions         ∷ Maybe (Array { say ∷ String, andThen ∷ SW Unit })
  -- first visible row of inventory
  , inventoryRow        ∷ Int
  -- timeouts - note we add to newTimeouts - which will then get copied over to wTimeouts as it count's them down
  --  TODO why? just so that new timeouts aren't updated immediately? Should we just add extra timeout?
  , timeouts            ∷ Array { timeout ∷ Milliseconds, andThen ∷ SW Unit }
  , newTimeouts         ∷ Array { timeout ∷ Milliseconds, andThen ∷ SW Unit }
  , canvasDim           ∷ { width :: Number, height :: Number }
  , cursorPos           ∷ Tuple Int Int
  }

derive instance newtypeWorld ∷ Newtype World _

type SW = State World


--------------------------------------------------------------------------------

data MkStateF1 next a = MkStateF1
  { val  ∷ a
  , next ∷ Sid a → next
  }

mapMkStateExists ∷ ∀ a b. (a → b) → Exists (MkStateF1 a) → Exists (MkStateF1 b)
mapMkStateExists f exists =
  runExists (\(MkStateF1 {val, next}) →
    mkExists $ MkStateF1 {val, next: f <<< next}
  ) exists

data StoryBuilderSyntax next
  = SetSTitle        String                             next
  | SetSDefaultVerb  Verb                               next
  | SetSInitialRoom  Rid                                next
  | SetSInitialActor Aid                                next
  | SetSInitial      (SW Unit)                          next
  | MkActor          Aid (ActorBuilder  Unit)           next
  | MkObject         Oid (ObjectBuilder Unit)           next
  | MkRoom           Rid (RoomBuilder   Unit)           next
  | MkState          String (Exists (MkStateF1          next))
  | MkImg            FilePath                  (Image → next)
  | MkVerb           String                    (Verb  → next)

-- -- we can't derive Functor with existential quantification
instance functorStoryBuilderSyntax ∷ Functor StoryBuilderSyntax where
    map f (SetSTitle t        next) = SetSTitle t        (f next)
    map f (SetSDefaultVerb v  next) = SetSDefaultVerb v  (f next)
    map f (SetSInitialRoom r  next) = SetSInitialRoom r  (f next)
    map f (SetSInitialActor a next) = SetSInitialActor a (f next)
    map f (SetSInitial     i  next) = SetSInitial i      (f next)
    map f (MkObject  oid ob   next) = MkObject  oid ob   (f next)
    map f (MkRoom    rid rb   next) = MkRoom    rid rb   (f next)
    map f (MkActor   aid ab   next) = MkActor   aid ab   (f next)
    map f (MkState   l      exists) = MkState   l (mapMkStateExists f exists)
    map f (MkImg     fp       next) = MkImg  fp          (f <<< next)
    map f (MkVerb    t        next) = MkVerb t           (f <<< next)


type StoryBuilder = Free StoryBuilderSyntax

type StoryDef = StoryBuilder Unit

data RoomBuilderSyntax next
  = SetRImage    (SW Image)                next
  | SetRHotspots (Array (Tuple Oid Poly))  next
  | SetRItems    (Array (Tuple Oid Point)) next
  | SetROnEnter  (SW Unit)                 next
  | SetROnExit   (SW Unit)                 next

derive instance functorRoomBuilderSyntax ∷ Functor RoomBuilderSyntax

type ActorBuilder = Free ActorBuilderSyntax

data ActorBuilderSyntax next
  = SetAName    String     next
  | SetAImage   (SW Image) next
  | SetAColor   Color      next

derive instance functorActorBuilderSyntax ∷ Functor ActorBuilderSyntax

type RoomBuilder = Free RoomBuilderSyntax

data ObjectBuilderSyntax next
  = SetOName           String                      next
  | SetOImage          (SW (Maybe Image))          next
  | SetOInventoryImage (SW (Maybe Image))          next
  | SetOSuggestedVerb  (SW Verb)                   next
  | SetOInitial        (SW Unit)                   next
  | SetOOnEnter        (SW Unit)                   next
  | SetOOnExit         (SW Unit)                   next
  | AddOInterceptor    (Array Verb) (SW Unit)      next
  | AddOInterceptor2   Verb String (Oid → SW Unit) next

derive instance functorObjectBuilderSyntax ∷ Functor ObjectBuilderSyntax

type ObjectBuilder = Free ObjectBuilderSyntax


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Add a title to Story.
setSTitle ∷ String → StoryBuilder Unit
setSTitle s = liftF $ SetSTitle s unit

setSDefaultVerb ∷ Verb → StoryBuilder Unit
setSDefaultVerb v = liftF $ SetSDefaultVerb v unit

setSInitialActor ∷ Aid → StoryBuilder Unit
setSInitialActor a = liftF $ SetSInitialActor a unit

setSInitialRoom ∷ Rid → StoryBuilder Unit
setSInitialRoom r = liftF $ SetSInitialRoom r unit

setSInitial ∷ SW Unit → StoryBuilder Unit
setSInitial i = liftF $ SetSInitial i unit

-- | Define a room.
-- e.g.
--
-- > mkRoom roomId $ do
-- >   setRTitle "Room"
mkRoom
  ∷ Rid
  -- ^ @room ref@ of room to be defined
  → RoomBuilder Unit
  -- ^ the room definition
  → StoryBuilder Unit
mkRoom rid' rb = liftF $ MkRoom rid' rb unit

-- | Define an object.
-- e.g.
--
-- > mkObject objId $ do
-- >    setOTitle "Object"
mkObject
  ∷ Oid
  -- ^ @object ref@ of object to be defined
  → ObjectBuilder Unit
  -- ^ the object definition
  → StoryBuilder Unit
mkObject oid' obj  = liftF $ MkObject oid' obj unit

-- | Define an Actor.
-- e.g.
--
-- > mkActor aid $ do
-- >   setAName "Name"
mkActor
  ∷ Aid
  -- ^ @actor ref@ of actor to be defined
  → ActorBuilder Unit
  -- ^ the actor definition
  → StoryBuilder Unit
mkActor aid' actor = liftF $ MkActor aid' actor unit

mkImg ∷ FilePath → StoryBuilder Image
mkImg fp = liftF $ MkImg fp identity

mkVerb ∷ String → StoryBuilder Verb
mkVerb t = liftF $ MkVerb t identity

-- | Introduce state with a label and initial value.
-- e.g.
--
-- > stateRef ← mkState "stateLabel" False
mkState ∷ ∀ a. String → a → StoryBuilder (Sid a)
mkState l val = liftF $ MkState l (mkExists $ MkStateF1 { val, next : identity })



setAImage ∷ SW Image → ActorBuilder Unit
setAImage img = liftF $ SetAImage img unit

setAName ∷ String → ActorBuilder Unit
setAName t = liftF $ SetAName t unit

setAColor ∷ Color → ActorBuilder Unit
setAColor color = liftF $ SetAColor color unit


-- | Add title to room.
setRImage ∷ SW Image → RoomBuilder Unit
setRImage img = liftF $ SetRImage img unit

setRHotspots ∷ Array (Tuple Oid Poly) → RoomBuilder Unit
setRHotspots h = liftF $ SetRHotspots h unit

-- | Add an item to room.
setRItems ∷ Array (Tuple Oid Point) → RoomBuilder Unit
setRItems items = liftF $ SetRItems items unit

setROnEnter ∷ SW Unit → RoomBuilder Unit
setROnEnter oe = liftF $ SetROnEnter oe unit

setROnExit ∷ SW Unit → RoomBuilder Unit
setROnExit oe = liftF $ SetROnExit oe unit



-- | Add a title to object.
setOName ∷ String → ObjectBuilder Unit
setOName name = liftF $ SetOName name unit

setOImage ∷ SW (Maybe Image) → ObjectBuilder Unit
setOImage img = liftF $ SetOImage img unit

setOInventoryImage ∷ SW (Maybe Image) → ObjectBuilder Unit
setOInventoryImage img = liftF $ SetOInventoryImage img unit

setOSuggestedVerb ∷ SW Verb → ObjectBuilder Unit
setOSuggestedVerb sv = liftF $ SetOSuggestedVerb sv unit

setOInitial ∷ SW Unit → ObjectBuilder Unit
setOInitial s = liftF $ SetOInitial s unit

setOOnEnter ∷ SW Unit → ObjectBuilder Unit
setOOnEnter oe = liftF $ SetOOnEnter oe unit

setOOnExit ∷ SW Unit → ObjectBuilder Unit
setOOnExit oe = liftF $ SetOOnExit oe unit

addOInterceptor ∷ Array Verb → SW Unit → ObjectBuilder Unit
addOInterceptor vs a = liftF $ AddOInterceptor vs a unit

addOInterceptor2 ∷ Verb → String → (Oid → SW Unit) → ObjectBuilder Unit
addOInterceptor2 v p a = liftF $ AddOInterceptor2 v p a unit
