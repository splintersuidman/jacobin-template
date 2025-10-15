module Jacobin.FinalSlide (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (Composite(..), getContext2D) as Canvas
import Graphics.Canvas (Dimensions, ScaleTransform, TextAlign(..), TextBaseline(..))
import Partial.Unsafe (unsafePartial)
import Sjablong.Layer (mkSomeLayer)
import Sjablong.Layer.Image (mkEmptyImageLayer, mkImageLayer)
import Sjablong.Layer.Image as ImageLayer
import Sjablong.Layer.Layers (mkLayers)
import Sjablong.Layer.Rectangle (mkRectangleLayer)
import Sjablong.Layer.Ref (mkRefLayer)
import Sjablong.Layer.Text.Markup (MarkupTextLayer(..))
import Sjablong.Layer.Text.Markup as Markup
import Sjablong.Layer.Undraggable (mkUndraggable, mkUndraggableHorizontal)
import Sjablong.Main (addEventListeners, connectObjectUrlInput, connectScaleRange, connectTextAreaPure, mkDownloadButton, mkTemplate, mkTemplateContext, redraw)

instagramDimensions :: Dimensions
instagramDimensions = { width: 1080.0, height: 1080.0 * 5.0 / 4.0 }

templateResolution :: Number
templateResolution = 2.0

templateResolutionScale :: ScaleTransform
templateResolutionScale = { scaleX: templateResolution, scaleY: templateResolution }

templateDimensions :: Dimensions
templateDimensions = { width: templateResolution, height: templateResolution } * instagramDimensions

templateWidth :: Number
templateWidth = templateDimensions.width

templateHeight :: Number
templateHeight = templateDimensions.height

main :: Effect Unit
main = void $ unsafePartial do
  Just templateContext <- mkTemplateContext "canvas" templateDimensions
  canvasContext <- Canvas.getContext2D templateContext.canvas

  guillotine <- mkImageLayer
    "./img/guillotine2x.png"
    { x: 60.0 * templateResolution, y: 60.0 * templateResolution }
    { scaleX: (1560.0 + 2.0 * 60.0) / 1560.0, scaleY: 1.0 }
    Canvas.SourceOver

  logo <- mkImageLayer
    "./img/jacobinlogo80x200.png"
    { x: templateWidth - (60.0 + 40.0) * templateResolution, y: templateHeight - (60.0 + 100.0) * templateResolution }
    { scaleX: 1.0, scaleY: 1.0 }
    Canvas.SourceOver

  imageLayer <- mkRefLayer =<< mkEmptyImageLayer { x: 0.0, y: 0.0 } { scaleX: 1.0, scaleY: 1.0 } Canvas.SourceOver
  connectObjectUrlInput templateContext "image" imageLayer ImageLayer.loadImage
  connectScaleRange templateContext "image-size" imageLayer

  textAboveLayer <- mkRefLayer $ MarkupTextLayer
    { text: []
    , lineHeight: 1.1
    , position: { x: templateWidth / 2.0, y: 150.0 * templateResolution }
    , fillStyle: "#f00"
    , font:
        { name: "Oswald"
        , style: { normal: "normal", italic: "italic" }
        , weight: { normal: "500", bold: "700" }
        , size: 50.0 * templateResolution
        }
    , align: AlignCenter
    , baseline: BaselineTop
    , letterSpacing: "-3px"
    , emptyLineHeight: 0.25
    , dragOffset: Nothing
    , maxWidth: Just $ templateWidth - (60.0 + 60.0 + 40.0 + 60.0) * templateResolution
    , context: canvasContext
    }
  connectTextAreaPure templateContext "text-above" textAboveLayer Markup.setText'

  textBelowLayer <- mkRefLayer $ MarkupTextLayer
    { text: []
    , lineHeight: 1.1
    , position: { x: templateWidth / 2.0, y: templateHeight - 150.0 * templateResolution }
    , fillStyle: "#f00"
    , font:
        { name: "Oswald"
        , style: { normal: "normal", italic: "italic" }
        , weight: { normal: "500", bold: "700" }
        , size: 50.0 * templateResolution
        }
    , align: AlignCenter
    , baseline: BaselineTop
    , letterSpacing: "-3px"
    , emptyLineHeight: 0.25
    , dragOffset: Nothing
    , maxWidth: Just $ templateWidth - (60.0 + 60.0 + 40.0 + 60.0) * templateResolution
    , context: canvasContext
    }
  connectTextAreaPure templateContext "text-below" textBelowLayer Markup.setText'

  let
    layers = mkLayers @Effect
      [ mkSomeLayer $ mkUndraggable guillotine
      , mkSomeLayer $ mkUndraggable logo
      , mkSomeLayer $ mkUndraggableHorizontal textAboveLayer
      , mkSomeLayer $ mkUndraggableHorizontal textBelowLayer
      , mkSomeLayer imageLayer
      , mkSomeLayer $ mkUndraggable $ mkRectangleLayer { x: 0.0, y: 0.0, width: templateWidth, height: templateHeight } "#fff"
      ]

  template <- mkTemplate templateContext layers
  redraw template
  addEventListeners template
  Just _ <- mkDownloadButton "download" "jacobin-slot.png" template
  pure unit
