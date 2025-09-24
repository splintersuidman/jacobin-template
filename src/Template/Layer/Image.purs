module Template.Layer.Image
  ( ImageLayer(..)
  , mkImageLayer
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (CanvasImageSource, Composite, ScaleTransform)
import Graphics.Canvas (setGlobalCompositeOperation, tryLoadImage, withContext) as Canvas
import Graphics.Canvas.Extra (canvasImageSourceDimensions, canvasImageSourceHeight, canvasImageSourceWidth, drawImageDimensions) as Canvas
import Template.Layer (class Layer, class Scalable, DragOffset, Point, dragTranslateMaybe, scaleDimensions, translatePoint)

newtype ImageLayer = ImageLayer
  { image :: Ref (Maybe CanvasImageSource)
  , position :: Point
  , scale :: ScaleTransform
  , composite :: Composite
  , dragOffset :: Maybe DragOffset
  }

mkImageLayer :: forall @m. MonadEffect m => String -> Point -> ScaleTransform -> Composite -> m ImageLayer
mkImageLayer path position scale composite = liftEffect $ do
  image <- Ref.new Nothing
  Canvas.tryLoadImage path $ flip Ref.modify_ image <<< const
  pure $ ImageLayer
    { image
    , position
    , scale
    , composite
    , dragOffset: Nothing
    }

instance MonadEffect m => Layer m ImageLayer where
  position (ImageLayer i) = pure i.position
  translate t (ImageLayer i) = pure $ ImageLayer i { position = translatePoint t i.position }
  containsPoint { x, y } (ImageLayer i) = do
    image <- liftEffect $ Ref.read i.image
    case image of
      Nothing -> pure false
      Just source -> pure
        $  i.position.x <= x && x <= i.position.x + Canvas.canvasImageSourceWidth source * i.scale.scaleX
        && i.position.y <= y && y <= i.position.y + Canvas.canvasImageSourceHeight source * i.scale.scaleY

  dragStart offset (ImageLayer i) = pure $ ImageLayer i { dragOffset = Just offset }
  drag t layer@(ImageLayer i) = dragTranslateMaybe i.dragOffset t layer
  dragEnd (ImageLayer i) = pure $ ImageLayer i { dragOffset = Nothing }

  draw ctx (ImageLayer i) = do
    image <- Ref.read i.image
    case image of
      Nothing -> pure unit
      Just source -> Canvas.withContext ctx do
        Canvas.setGlobalCompositeOperation ctx i.composite
        Canvas.drawImageDimensions ctx source i.position $
          scaleDimensions i.scale $ Canvas.canvasImageSourceDimensions source

instance MonadEffect m => Scalable m ImageLayer where
  scale s (ImageLayer i) = pure $ ImageLayer i { scale = s * i.scale }
