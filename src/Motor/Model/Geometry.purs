module Motor.Model.Geometry where

import Prelude
import Data.Array as A
import Data.Foldable (all)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), uncurry)

type Edge   = Tuple Point Point
type Point  = Tuple Number Number
type Poly   = Array Point
type Vector = Tuple Number Number

crossProduct ∷ Vector → Vector → Number
crossProduct (Tuple x1 y1) (Tuple x2 y2) = (x1*y2) - (y1*x2)

-- Transform a list of points into a list of edges
polyEdges ∷ Poly → Array Edge
polyEdges p =
  fromMaybe [] do ht1 ← A.uncons p
                  ht2 ← A.uncons ht1.tail
                  pure $ (Tuple ht1.head ht2.head) `A.cons` polyEdges ht1.tail

-- Compute the two vectors on which we'll compute the cross product
vectors ∷ Edge → Point → Tuple Vector Vector
vectors (Tuple (Tuple x1 y1) (Tuple x2 y2)) (Tuple px py) =
  Tuple (Tuple (x2 - x1) (y2 - y1)) (Tuple (px - x1) (py - y1))

-- Indicate whether the point is within the convex polygon
pointInPoly ∷ Poly → Point → Boolean
pointInPoly poly p =
    all (_ >= 0.0) normals
  where
    normals = map (\edge → (uncurry crossProduct) (vectors edge p)) (polyEdges poly)

rect ∷ Point → Int → Int → Poly
rect (Tuple px py) w h =
    map translate [ Tuple 0.0 0.0
                  , Tuple w'  0.0
                  , Tuple w'  h'
                  , Tuple 0.0 h'
                  , Tuple 0.0 0.0
                  ]
  where
    translate (Tuple x y) = Tuple (px + x) (py + y)
    w' = toNumber w
    h' = toNumber h
