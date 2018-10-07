module Motor.Util
  ( ifNot
  , arrange
  ) where

import Prelude
import Data.Array as A
import Data.List.Lazy as L
import Data.Maybe (Maybe(..), maybe, fromMaybe)

ifNot ∷ ∀ a. Maybe a → Maybe a → Maybe a
ifNot a b = maybe b Just a

-- arrange in n columns
arrange ∷ ∀ a b. Int → (a → Int → Int → b) → Array a → Array b
arrange numCols f as =
  zipWith3 f as (map (_ `mod` numCols) (A.range 0 100)) (A.concatMap (A.fromFoldable <<< L.take numCols <<< L.repeat) (A.range 0 100))


zipWith3 ∷ ∀ a b c d. (a → b → c → d) → Array a → Array b → Array c → Array d
zipWith3 z = go
  where
    go as bs cs = fromMaybe [] do
      a ← A.uncons as
      b ← A.uncons bs
      c ← A.uncons cs
      pure $ z a.head b.head c.head `A.cons` go a.tail b.tail c.tail
