module Motor.View.Canvas
  ( Picture
  , Shape
  , Bitmap
  , Canvas
  , Color(..)
  , createCanvas
  , render
  , renderOnTop
  , buffer
  , setStrokeColor
  , setFillColor
  , opacity
  , color
  , lineWidth
  , translate
  , rotate
  , scale
  , fill
  , stroke
  , clip
  , path
  , line
  , rect
  , circle
  , arc
  , font
  , text
  , draw
  , bitmapDim
  ) where

import Prelude

import Data.Array as A
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D)
import Graphics.Canvas as C
import Math (pi)
import Motor.Model.Geometry as G

type Bitmap = CanvasImageSource
type Canvas = CanvasElement
type Angle  = Number
data Color = RGB  Int Int Int
           | RGBA Int Int Int Int

newtype Picture a = Picture (Context2D → Effect a)

derive instance newtypePicture ∷ Newtype (Picture a) _

-- | A shape which can be either stroked or filled to yield a picture.
newtype Shape a = Shape (Context2D → Effect a)

derive instance newtypeShape ∷ Newtype (Shape a) _

instance functorPicture ∷ Functor Picture where
  map f p = Picture $ \ctx →
    unwrap p ctx >>= pure <<< f

instance applyPicture ∷ Apply Picture where
  apply pfab pa = Picture $ \ctx → do
    fab ← unwrap pfab ctx
    a   ← unwrap pa   ctx
    pure (fab a)


instance applicativePicture ∷ Applicative Picture where
  pure a = Picture $ \_ → pure a

instance bindPicture ∷ Bind Picture where
  bind (Picture m) f = Picture $ \ctx → do
    x ← m ctx
    unwrap (f x) ctx

instance monadPicture ∷ Monad Picture

instance functorShape ∷ Functor Shape where
  map f s = Shape $ \ctx →
    unwrap s ctx >>= pure <<< f

instance applyShape ∷ Apply Shape where
  apply sfab sa = Shape $ \ctx → do
    fab ← unwrap sfab ctx
    a   ← unwrap sa   ctx
    pure (fab a)

instance applicativeShape ∷ Applicative Shape where
  pure a = Shape $ \_ → pure a

instance bindShape ∷ Bind Shape where
  bind (Shape m) f = Shape $ \ctx → do
    x ← m ctx
    unwrap (f x) ctx

instance monadShape ∷ Monad Shape

foreign import jsResetCanvas ∷ Canvas → Effect Unit

foreign import bitmapWidth  ∷ Bitmap → Effect Int
foreign import bitmapHeight ∷ Bitmap → Effect Int

color2JSString ∷ Color → String
color2JSString (RGB  r g b  ) =
  "rgb(" <> show r <> "," <> show g <> "," <> show b <> ")"
color2JSString (RGBA r g b a) =
  "rgba(" <> show r <> "," <> show g <> "," <> show b <> "," <> show a <> ")"

foreign import jsCreateCanvas ∷ String → Effect Canvas

createCanvas ∷ Int → Int → Effect Canvas
createCanvas w h = do
  c ← jsCreateCanvas "canvas"
  C.setCanvasWidth c $ toNumber w
  C.setCanvasHeight c $ toNumber h
  pure c

render ∷ ∀ a. Canvas → Picture a → Effect a
render canvas (Picture p) = do
  jsResetCanvas canvas
  ctx ← C.getContext2D canvas
  p ctx

-- | Draw a picture onto a canvas without first clearing it.
renderOnTop ∷ ∀ a. Context2D → Picture a → Effect a
renderOnTop ctx (Picture p) = p ctx

-- | Generate a data URL from the contents of a canvas.
-- toDataURL ∷ Canvas → Effect URL
-- toDataURL (Canvas _ el) = do
--   fromJSString <$> jsCanvasToDataURL el

-- | Create a new off-screen buffer and store the given picture in it.
buffer ∷ Int → Int → Picture Unit → Effect Bitmap
buffer w h pict = do
  c ← createCanvas w h
  render c pict
  pure $ C.canvasElementToImageSource c



-- | Set a new color for strokes.
setStrokeColor ∷ Color → Picture Unit
setStrokeColor c = Picture $ \ctx → do
  setProp ctx "strokeStyle" (color2JSString c)

-- | Set a new fill color.
setFillColor ∷ Color → Picture Unit
setFillColor c = Picture $ \ctx → do
  setProp ctx "fillStyle" (color2JSString c)

-- | Draw a picture with the given opacity.
opacity ∷ Number → Picture Unit → Picture Unit
opacity alpha (Picture pict) = Picture $ \ctx → do
  alpha' ← getProp ctx "globalAlpha"
  setProp ctx "globalAlpha" (show alpha)
  pict ctx
  setProp ctx "globalAlpha" (show alpha')

-- | Draw the given Picture using the specified Color for both stroke and fill,
--   then restore the previous stroke and fill colors.
color ∷ Color → Picture Unit → Picture Unit
color c (Picture pict) = Picture $ \ctx → do
    fc ← getProp ctx "fillStyle"
    sc ← getProp ctx "strokeStyle"
    setProp ctx "fillStyle" c'
    setProp ctx "strokeStyle" c'
    pict ctx
    setProp ctx "fillStyle" fc
    setProp ctx "strokeStyle" sc
  where
    c' = color2JSString c

-- | Draw the given picture using a new line width.
lineWidth ∷ Number → Picture Unit → Picture Unit
lineWidth w (Picture pict) = Picture $ \ctx → do
  lw ← getProp ctx "lineWidth"
  setProp ctx "lineWidth" (show w)
  pict ctx
  setProp ctx "lineWidth" (show lw)

-- | Draw the specified picture using the given point as the origin.
translate ∷ G.Vector → Picture Unit → Picture Unit
translate (Tuple x y) (Picture pict) = Picture $ \ctx → do
  C.save ctx
  C.translate ctx { translateX: x, translateY: y }
  pict ctx
  C.restore ctx

-- | Draw the specified picture rotated @r@ radians clockwise.
rotate ∷ Number → Picture Unit → Picture Unit
rotate rad (Picture pict) = Picture $ \ctx → do
  C.save ctx
  C.rotate ctx rad
  pict ctx
  C.restore ctx

-- | Draw the specified picture scaled as specified by the scale vector.
scale ∷ G.Vector → Picture Unit → Picture Unit
scale (Tuple x y) (Picture pict) = Picture $ \ctx → do
  C.save ctx
  C.scale ctx { scaleX : x, scaleY: y }
  pict ctx
  C.restore ctx

-- | Draw a filled shape.
fill ∷ Shape Unit → Picture Unit
fill (Shape shape) = Picture $ \ctx → do
  C.beginPath ctx
  shape ctx
  C.fill ctx

-- | Draw the contours of a shape.
stroke ∷ Shape Unit → Picture Unit
stroke (Shape shape) = Picture $ \ctx → do
  C.beginPath ctx
  shape ctx
  C.stroke ctx

-- | Draw a picture clipped to the given path.
clip ∷ Shape Unit → Picture Unit → Picture Unit
clip (Shape shape) (Picture pict) = Picture $ \ctx → do
  C.save ctx
  C.beginPath ctx
  shape ctx
  C.clip ctx
  pict ctx
  C.restore ctx

-- | Draw a path along the specified points.
path ∷ Array G.Point → Shape Unit
path p = case A.uncons p of
  Just {head, tail} → Shape $ \ctx → do
    let Tuple x1 y1 = head
    C.moveTo ctx x1 y1
    traverse_ (uncurry $ C.lineTo ctx) tail
  Nothing → pure unit


-- | Draw a line between two points.
line ∷ G.Point → G.Point → Shape Unit
line p1 p2 = path [p1, p2]

-- | Draw a rectangle between the two given points.
rect ∷ G.Point → G.Point → Shape Unit
rect (Tuple x1 y1) (Tuple x2 y2) = path [ Tuple x1 y1
                                        , Tuple x2 y1
                                        , Tuple x2 y2
                                        , Tuple x1 y2
                                        , Tuple x1 y1
                                        ]

-- | Draw a circle shape.
circle ∷ G.Point → Number → Shape Unit
circle (Tuple x y) radius = Shape $ \ctx → do
  C.moveTo ctx (x + radius) y
  C.arc ctx { x, y, radius, start : 0.0, end : 2.0 * pi }

-- | Draw an arc. An arc is specified as a drawn portion of an imaginary
--   circle with a center point, a radius, a starting angle and an ending
--   angle.
--   For instance, @arc (0, 0) 10 0 pi@ will draw a half circle centered at
--   (0, 0), with a radius of 10 pixels.
arc ∷ G.Point → Number → Angle → Angle → Shape Unit
arc (Tuple x y) radius start end = Shape $ \ctx →
  C.arc ctx { x, y, radius, start, end }

-- | Draw a picture using a certain font. Obviously only affects text.
font ∷ String → Picture Unit → Picture Unit
font f (Picture pict) = Picture $ \ctx → do
  f' ← getProp ctx "font"
  setProp ctx "font" f
  pict ctx
  setProp ctx "font" f'

-- | Draw some text onto the canvas.
text ∷ G.Point → String → Picture Unit
text (Tuple x y) str = Picture $ \ctx →
  C.fillText ctx str x y

-- | Draw the image buffer with its top left corner at the specified point.
draw ∷ Bitmap → G.Point → Picture Unit
draw bm (Tuple x y) = Picture $ \ctx →
  C.drawImage ctx bm x y


bitmapDim ∷ Bitmap → Effect (Tuple Int Int)
bitmapDim bm = do
  wi ← bitmapWidth bm
  hi ← bitmapHeight bm
  pure (Tuple wi hi)


foreign import setProp ∷ ∀ a. a → String → String → Effect Unit

foreign import getProp ∷ ∀ a. a → String → Effect String
