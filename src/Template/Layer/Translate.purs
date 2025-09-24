module Template.Layer.Translate
  ( TranslateLayer(..)
  , mkTranslateLayer
  ) where

import Prelude

import Graphics.Canvas (TranslateTransform)
import Graphics.Canvas as Canvas
import Template.Layer (class Layer, class LayerWrapper, class Scalable, containsPoint, drag, dragEnd, dragStart, draw, mapLayerWrapper, position, scale, translatePoint)

newtype TranslateLayer (m :: Type -> Type) l = TranslateLayer
  { translation :: TranslateTransform
  , layer :: l
  }

derive instance Functor (TranslateLayer m)

instance LayerWrapper TranslateLayer where
  mapLayerWrapper f (TranslateLayer l) = f l.layer <#> \layer -> TranslateLayer l { layer = layer }

mkTranslateLayer :: forall @m l. TranslateTransform -> l -> TranslateLayer m l
mkTranslateLayer translation layer = TranslateLayer { translation, layer }

instance (Monad m, Layer m l) => Layer m (TranslateLayer m l) where
  position (TranslateLayer l) = translatePoint l.translation <$> position l.layer
  translate translation (TranslateLayer l) = pure $ TranslateLayer l { translation = translation + l.translation }
  containsPoint p (TranslateLayer l) = containsPoint (translatePoint (-l.translation) p) l.layer
  dragStart = mapLayerWrapper <<< dragStart
  drag = mapLayerWrapper <<< drag
  dragEnd = mapLayerWrapper dragEnd

  draw ctx (TranslateLayer l) = Canvas.withContext ctx $ do
    Canvas.translate ctx l.translation
    draw @m ctx l.layer

instance (Monad m, Scalable m l) => Scalable m (TranslateLayer m l) where
  scale = mapLayerWrapper <<< scale
