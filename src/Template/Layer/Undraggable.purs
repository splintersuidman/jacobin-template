module Template.Layer.Undraggable
  ( Undraggable(..)
  , mkUndraggable
  ) where

import Prelude

import Template.Layer (class Layer, class LayerWrapper, class Scalable, containsPoint, draw, mapLayerWrapper, position, scale, translate)

newtype Undraggable (m :: Type -> Type) l = Undraggable l

derive instance Functor (Undraggable m)

instance LayerWrapper Undraggable where
  mapLayerWrapper f (Undraggable l) = Undraggable <$> f l

mkUndraggable :: forall @m l. l -> Undraggable m l
mkUndraggable = Undraggable

instance (Monad m, Layer m l) => Layer m (Undraggable m l) where
  position (Undraggable l) = position l
  translate = mapLayerWrapper <<< translate
  containsPoint p (Undraggable l) = containsPoint p l
  dragStart = const pure
  drag = const pure
  dragEnd = pure
  draw ctx (Undraggable l) = draw @m ctx l

instance (Monad m, Scalable m l) => Scalable m (Undraggable m l) where
  scale = mapLayerWrapper <<< scale
