module Template.Layer.Undraggable
  ( Undraggable(..)
  , mkUndraggable
  ) where

import Prelude

import Template.Layer (class Layer, class LayerWrapper, class Scalable, containsPoint, draw, mapLayerWrapper, position, scale, translate)

newtype Undraggable l = Undraggable l

derive instance Functor Undraggable

instance LayerWrapper Undraggable where
  mapLayerWrapper f (Undraggable l) = Undraggable <$> f l

mkUndraggable :: forall l. l -> Undraggable l
mkUndraggable = Undraggable

instance (Monad m, Layer m l) => Layer m (Undraggable l) where
  position (Undraggable l) = position l
  translate = mapLayerWrapper <<< translate
  containsPoint p (Undraggable l) = containsPoint p l
  dragStart = const pure
  drag = const pure
  dragEnd = pure
  draw ctx (Undraggable l) = draw @m ctx l

instance (Monad m, Scalable m l, Layer m l) => Scalable m (Undraggable l) where
  scale = mapLayerWrapper <<< scale
