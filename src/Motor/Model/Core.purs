module Motor.Model.Core where

import Prelude
import Control.Monad.Except (Except, except, runExcept)
import Control.Monad.Free (runFreeM)
import Control.Monad.State (State, StateT, get, modify_, execState, execStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array as A
import Data.Either (Either, note)
import Data.Exists (runExists)
import Data.Foldable (traverse_)
import Data.Lens ((^.))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign (unsafeToForeign)
import Motor.Model.Lens
import Motor.Model.Types
import Record as R


toStory ∷ ∀ next. Show next ⇒ StoryBuilder next → Either String Story
toStory sb = do
  let story0 =
        { title        : Nothing
        , verbs        : []
        , defaultVerb  : Nothing
        , currentRoom  : Nothing
        , currentActor : Nothing
        , rooms        : M.empty
        , objects      : M.empty
        , actors       : M.empty
        , images       : []
        , initial      : pure unit
        , states       : M.empty
        , inventory    : []
        }
  story1       ← runExcept $ execStateT (runFreeM toStory' sb) story0
  title        ← note "story title not set"         story1.title
  defaultVerb  ← note "story default verb not set"  story1.defaultVerb
  currentRoom  ← note "story current room not set"  story1.currentRoom
  currentActor ← note "story current actor not set" story1.currentActor
  pure $ Story $ R.merge {title, defaultVerb, currentRoom, currentActor } story1

toStory' ∷ ∀ next. StoryBuilderSyntax (StoryBuilder next) → StateT _ (Except String) (StoryBuilder next)
toStory' (SetSTitle s next) = do
  modify_ _ { title = Just s }
  pure next
toStory' (MkVerb label g) = do
  let v = Verb { label }
  modify_ \s → s { verbs = A.cons v s.verbs }
  pure $ g v
toStory' (SetSDefaultVerb v next) = do
  modify_ _ { defaultVerb = Just v }
  pure next
toStory' (SetSInitialRoom r next) = do
  modify_ _ { currentRoom = Just r }
  pure next
toStory' (SetSInitialActor a next) = do
  modify_ _ { currentActor = Just a }
  pure next
toStory' (SetSInitial i next) = do
  modify_ _ { initial = i }
  pure next
toStory' (MkObject oid' ob next) = do
  obj ← lift $ except $ buildObject oid' ob
  modify_ \s → s { objects = M.insert oid' obj s.objects }
  pure next
toStory' (MkRoom rid' rb next) = do
  room ← lift $ except $ buildRoom rid' rb
  modify_ \s → s { rooms = M.insert rid' room s.rooms }
  pure next
toStory' (MkActor aid' ab next) = do
  actor ← lift $ except $ buildActor aid' ab
  modify_ \s → s { actors = M.insert aid' actor s.actors }
  pure next
toStory' (MkState sidK exists) = do
  -- we could generate label instead of client passing in (e.g. UUID)
  let {f, a } = runExists (\(MkStateF1 {val, next}) →
                            { f: unsafeToForeign val, a: next (Sid sidK) }
                          ) exists
  modify_ \s → s { states = M.insert sidK f s.states }
  pure $ a
toStory' (MkImg filepath g) = do
  let img = Image { filepath }
  modify_ \s → s { images = A.cons img s.images }
  pure $ g img


--------------------------------------------------------------------------------

buildActor ∷ ∀ next. Aid → ActorBuilder next → Either String Actor
buildActor aid ab = do
  let actor0 =
        { name     : Nothing
        , color    : Nothing
        , image    : Nothing
        }
      actor1 = execState (runFreeM buildActor' ab) actor0
  name  ← note ("actor " <> show aid <> " name not set" ) actor1.name
  color ← note ("actor " <> show aid <> " color not set") actor1.color
  -- image ← note ("actor " <> show aid <> " image not set") actor1.image
  pure $ Actor
    { aid    : aid
    , name
    , color
    -- , image
    }

buildActor' ∷ ∀ next. ActorBuilderSyntax (ActorBuilder next) → State _ (ActorBuilder next)
buildActor' (SetAName name next) = do
  modify_ _ { name = Just name }
  pure next
buildActor' (SetAColor color next) = do
  modify_ _ { color = Just color }
  pure next
buildActor' (SetAImage img next) = do
  modify_ _ { image = Just img }
  pure next

--------------------------------------------------------------------------------


buildRoom ∷ ∀ next. Rid → RoomBuilder next → Either String Room
buildRoom rid rb = do
  let room0 =
        { image      : Nothing
        , hotspots   : []
        , items      : []
        , onEnter    : pure unit
        , onExit     : pure unit
        , state      : M.empty
        }
      room1 = execState (runFreeM buildRoom' rb) room0
  image ← note ("Room " <> show rid <> " image not defined") room1.image
  pure $ Room $ R.merge {rid, image, visitCount: 0 } room1


buildRoom' ∷ ∀ next. RoomBuilderSyntax (RoomBuilder next) → State _ (RoomBuilder next)
buildRoom' (SetRImage img next) = do
  modify_ _ { image = Just img }
  pure next
buildRoom' (SetRHotspots h next) = do
  modify_ _ { hotspots = h }
  pure next
buildRoom' (SetRItems items next) = do
  modify_ _ { items = items }
  pure next
buildRoom' (SetROnEnter oe next) = do
  modify_ _ { onEnter = oe }
  pure next
buildRoom' (SetROnExit oe next) = do
  modify_ _ { onExit = oe }
  pure next

--------------------------------------------------------------------------------

buildObject ∷ ∀ next. Oid → ObjectBuilder next → Either String Object
buildObject oid ob = do
  let obj0 =
       { name           : Nothing
       , interceptors   : M.empty
       , image          : pure Nothing
       , inventoryImage : pure Nothing
       , suggestedVerb  : Nothing
       , initial        : pure unit
       , onEnter        : pure unit
       , onExit         : pure unit
       }
      obj1 = execState (runFreeM buildObject' ob) obj0
  name          ← note ("Object " <> show oid <> " name not defined"         ) obj1.name
  suggestedVerb ← note ("Object " <> show oid <> " suggestedVerb not defined") obj1.suggestedVerb
  pure $ Object $ R.merge {oid, name, suggestedVerb } obj1

buildObject' ∷ ∀ next. ObjectBuilderSyntax (ObjectBuilder next) → State _ (ObjectBuilder next)
buildObject' (SetOName name next) = do
  modify_ _ { name = Just name }
  pure next
buildObject' (SetOImage img next) = do
  modify_ _ { image = img }
  pure next
buildObject' (SetOInventoryImage img next) = do
  modify_ _ { inventoryImage = img }
  pure next
buildObject' (SetOSuggestedVerb sv next) = do
  modify_ _ { suggestedVerb = Just sv }
  pure next
buildObject' (SetOInitial s next) = do
  modify_ _ { initial = s }
  pure next
buildObject' (SetOOnEnter oe next) = do
  modify_ _ { onEnter = oe }
  pure next
buildObject' (SetOOnExit oe next) = do
  modify_ _ { onExit = oe }
  pure next
buildObject' (AddOInterceptor vs routine1 next) = do
  let interceptors = M.fromFoldable $ map (\v → Tuple v (ObjRoutine1 {routine1 })) vs
  modify_ \s → s { interceptors = interceptors <> s.interceptors }
  pure next
buildObject' (AddOInterceptor2 v prep routine2 next) = do
  let interceptors = M.fromFoldable $ A.singleton (Tuple v (ObjRoutine2 {prep, routine2 }))
  modify_ \s → s { interceptors = interceptors <> s.interceptors }
  pure next

--------------------------------------------------------------------------------

-- | Initial game world.
initialWorld ∷ Story → Graphics → Effect World
initialWorld s g = do
  let w = World
        { story               : s
        , graphics            : g
        , selectedVerb        : Just $ s ^. sDefaultVerb
        , mouseOverObject     : Nothing
        , selectedObject      : Nothing
        , mouseOverVerb       : Nothing
        , mouseOverTextOption : Nothing
        , mouseOverInvArrow   : Nothing
        , displayText         : Nothing
        , gameOver            : false
        , actionCountMs       : Nothing
        , talkOptions         : Nothing
        , inventoryRow        : 0
        , timeouts            : []
        , newTimeouts         : []
        , canvasDim           : { width: 0.0, height: 0.0}
        , cursorPos           : Tuple 0 0
        }
  pure $ (flip execState) w do
    get >>= (_ ^. wStory <<< sInitial)
    get >>= traverse_ (_ ^. oInitial) <<< M.values <<< (_ ^. wStory <<< sObjects)
