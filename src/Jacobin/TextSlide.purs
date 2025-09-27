module Jacobin.TextSlide (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Effect (Effect)
import Graphics.Canvas (Composite(..), getContext2D) as Canvas
import Graphics.Canvas (Dimensions, ScaleTransform, TextAlign(..), TextBaseline(..))
import Partial.Unsafe (unsafePartial)
import Template.Layer (mkSomeLayer)
import Template.Layer.Image (mkImageLayer)
import Template.Layer.Layers (mkLayers)
import Template.Layer.Rectangle (mkRectangleLayer)
import Template.Layer.Ref (mkRefLayer)
import Template.Layer.Text (TextLayer(..), setText)
import Template.Layer.Text.Markup (MarkupTextLayer(..))
import Template.Layer.Text.Markup as Markup
import Template.Layer.Undraggable (mkUndraggable)
import Template.Main (addEventListeners, connectInputPure, connectMarkupTextSizeRange, connectTextAreaPure, mkDownloadButton, mkTemplate, mkTemplateContext, redraw)

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
    "./img/guillotine.svg"
    { x: 60.0 * templateResolution, y: 60.0 * templateResolution }
    templateResolutionScale
    Canvas.SourceOver

  logo <- mkImageLayer
    "./img/jacobinlogo.svg"
    { x: templateDimensions.width - 120.0 * templateResolution, y: templateDimensions.height - 200.0 * templateResolution }
    templateResolutionScale
    Canvas.SourceOver

  bodyTextLayer <- mkRefLayer $ MarkupTextLayer
    { text: []
    , lineHeight: 0.95
    , position: { x: 60.0 * templateResolution, y: 150.0 * templateResolution }
    , fillStyle: "#f00"
    , font:
        { name: "Oswald"
        , style: { normal: "normal", italic: "italic" }
        , weight: { normal: "500", bold: "700" }
        , size: 90.0 * templateResolution
        }
    , align: AlignLeft
    , baseline: BaselineTop
    , letterSpacing: "-3px"
    , dragOffset: Nothing
    , maxWidth: Just $ templateDimensions.width - 2.0 * 60.0 * templateResolution
    , context: canvasContext
    }
  connectTextAreaPure templateContext "bodytext" bodyTextLayer Markup.setText'
  connectMarkupTextSizeRange templateContext "bodytext-size" bodyTextLayer

  authorLayer <- mkRefLayer $ TextLayer
    { text: "AUTEUR"
    , lineHeight: 0.95
    , position: { x: 60.0 * templateResolution, y: templateDimensions.height - 200.0 * templateResolution }
    , fillStyle: "#f00"
    , fontName: "Oswald"
    , fontStyle: "normal"
    , fontWeight: "400"
    , fontSize: 50.0 * templateResolution
    , align: AlignLeft
    , baseline: BaselineTop
    , letterSpacing: "-3px"
    , dragOffset: Nothing
    , maxWidth: Nothing
    , context: canvasContext
    }

  connectInputPure templateContext "author" authorLayer (setText <<< toUpper)

  titleLayer <- mkRefLayer $ MarkupTextLayer
    { text: []
    , lineHeight: 0.9
    , position: { x: 60.0 * templateResolution, y: templateDimensions.height - 200.0 * templateResolution + 50.0 * templateResolution }
    , maxWidth: Just $ templateDimensions.width - 4.0 * 60.0 * templateResolution
    , fillStyle: "#f00"
    , font:
        { name: "Oswald"
        , style: { normal: "normal", italic: "italic" }
        , weight: { normal: "700", bold: "700" }
        , size: 50.0 * templateResolution
        }
    , align: AlignLeft
    , baseline: BaselineTop
    , letterSpacing: "-2px"
    , dragOffset: Nothing
    , context: canvasContext
    }
  connectTextAreaPure templateContext "title" titleLayer Markup.setText'

  let
    layers = mkUndraggable $ mkLayers @Effect
      [ mkSomeLayer guillotine
      , mkSomeLayer logo
      , mkSomeLayer authorLayer
      , mkSomeLayer titleLayer
      , mkSomeLayer bodyTextLayer
      , mkSomeLayer $ mkRectangleLayer { x: 0.0, y: 0.0, width: templateWidth, height: templateHeight } "#fff"
      ]

  template <- mkTemplate templateContext layers
  redraw template
  addEventListeners template
  Just _ <- mkDownloadButton "download" "jacobin.png" template
  pure unit
