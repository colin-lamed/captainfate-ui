module Motor.View.Update
  ( stepWorld
  ) where

import Prelude
import Control.Monad.State (StateT(..), get, gets, modify, runStateT)
import Data.Array as A
import Data.Identity (Identity)
import Data.Foldable (foldr)
import Data.Lens ((.~), (^.), (.=), (%=))
import Data.Lens.Prism.Maybe (_Just)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Motor.Model.Common (clearSelection)
import Motor.Model.Lens
import Motor.Model.Types (SW, World)
import Motor.View.Draw   (ImgCache, clearCache)


-- | Advance the state one iteration
stepWorld ∷ Milliseconds → ImgCache → StateT World Effect Unit
stepWorld dt imgCache = do
  updateTextCount dt imgCache
  updateTimeouts  dt imgCache
  updateAction    dt

updateTextCount ∷ Milliseconds → ImgCache → StateT World Effect Unit
updateTextCount dt imgCache =
  gets (_ ^. wDisplayText) >>= case _ of
    Nothing                                   → pure unit
    Just displayText
     | displayText ^. dtMs > Milliseconds 0.0 → (wDisplayText <<< _Just <<< dtMs) %= (\i → Milliseconds $ unwrap i - unwrap dt)
     | otherwise                              → do wDisplayText .= Nothing
                                                   hoist generalize $ displayText ^. dtAndThen
                                                   liftEffect $ clearCache imgCache

updateTimeouts ∷ Milliseconds → ImgCache → StateT World Effect Unit
updateTimeouts dt imgCache = do
    updatedTimeouts ← gets (_ ^. wTimeouts) >>= (map A.catMaybes <<< traverse doOrUpdate)
    newTimeouts     ← gets (_ ^. wNewTimeouts)
    wTimeouts    .= (updatedTimeouts <> newTimeouts)
    wNewTimeouts .= []
  where
    doOrUpdate { timeout, andThen }
      | timeout > Milliseconds 0.0 = pure $ Just { timeout : Milliseconds $ unwrap timeout - unwrap dt, andThen }
      | otherwise                  = do hoist generalize andThen
                                        liftEffect $ clearCache imgCache
                                        pure Nothing

-- TODO refactor similarity between updateTimeouts and updateAction
updateAction ∷ Milliseconds → StateT World Effect Unit
updateAction dt = do
  wActionCountMs %= map (\i → Milliseconds $ max (unwrap i - unwrap dt) 0.0)
  isTimeout ← gets $ (_ == Just (Milliseconds 0.0)) <<< (_ ^. wActionCountMs)
  when (isTimeout) do
    hoist generalize clearSelection
    wActionCountMs .= Nothing


-- TODO by making utils use MonadState, can avoid hoist
hoist ∷ ∀ m m' a s. (m ~> m') → StateT s m a → StateT s m' a
hoist f m = StateT (f <<< runStateT m)

generalize ∷ ∀ m. Monad m ⇒ Identity ~> m
generalize = pure <<< unwrap
