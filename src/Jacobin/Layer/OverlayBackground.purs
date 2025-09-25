module Jacobin.Layer.OverlayBackground
  ( OverlayBackgroundLayer(..)
  , mkOverlayBackgroundLayer
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number as Number
import Graphics.Canvas (Dimensions)
import Graphics.Canvas as Canvas
import Template.Layer (class Layer, DragOffset, Point, dragTranslateMaybe, translatePoint)

degreesToRadians :: Number -> Number
degreesToRadians degrees = degrees * Number.pi / 180.0

newtype OverlayBackgroundLayer = OverlayBackgroundLayer
  { position :: Point
  , dimensions :: Dimensions
  , angle :: Number
    -- ^ Angle in degrees
  , fillStyle :: String
  , dragOffset :: Maybe DragOffset
  }

mkOverlayBackgroundLayer :: Point -> Dimensions -> Number -> String -> OverlayBackgroundLayer
mkOverlayBackgroundLayer position dimensions angle fillStyle = OverlayBackgroundLayer
  { position
  , dimensions
  , angle
  , fillStyle
  , dragOffset: Nothing
  }

instance Monad m => Layer m OverlayBackgroundLayer where
  position (OverlayBackgroundLayer layer) = pure layer.position
  translate translation (OverlayBackgroundLayer layer) = pure $ OverlayBackgroundLayer layer { position = translatePoint translation layer.position }

  containsPoint { x, y } (OverlayBackgroundLayer layer) = do
    let offsetX = (layer.dimensions.height - y) / Number.tan (degreesToRadians layer.angle)
    pure $ layer.position.x + offsetX <= x && x <= layer.position.x + layer.dimensions.width
        && layer.position.y <= y && y <= layer.position.y + layer.dimensions.height

  draw ctx (OverlayBackgroundLayer layer) = Canvas.withContext ctx do
    Canvas.setFillStyle ctx layer.fillStyle
    Canvas.beginPath ctx
    -- Bottom left
    Canvas.moveTo ctx layer.position.x (layer.position.y + layer.dimensions.height)
    -- Bottom right
    Canvas.lineTo ctx (layer.position.x + layer.dimensions.width) (layer.position.y + layer.dimensions.height)
    -- Top right 
    Canvas.lineTo ctx (layer.position.x + layer.dimensions.width) layer.position.y
    -- Top left
    Canvas.lineTo ctx (layer.position.x + layer.dimensions.height / Number.tan (degreesToRadians layer.angle)) layer.position.y
    Canvas.closePath ctx
    Canvas.fill ctx

  dragStart offset (OverlayBackgroundLayer layer) = pure $ OverlayBackgroundLayer layer { dragOffset = Just offset }
  drag translation l@(OverlayBackgroundLayer layer) = dragTranslateMaybe layer.dragOffset translation l
  dragEnd (OverlayBackgroundLayer layer) = pure $ OverlayBackgroundLayer layer { dragOffset = Nothing }
