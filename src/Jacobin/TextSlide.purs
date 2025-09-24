module Jacobin.TextSlide (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Effect (Effect)
import Graphics.Canvas (Composite(..), getContext2D) as Canvas
import Graphics.Canvas (Dimensions, TextAlign(..), TextBaseline(..))
import Partial.Unsafe (unsafePartial)
import Template.Layer (mkSomeLayer)
import Template.Layer.Image (mkImageLayer)
import Template.Layer.Layers (mkLayers)
import Template.Layer.Rectangle (mkRectangleLayer)
import Template.Layer.Ref (mkRefLayer)
import Template.Layer.Text (TextLayer(..), setText)
import Template.Layer.Undraggable (mkUndraggable)
import Template.Main (addEventListeners, connectInputPure, connectTextAreaPure, connectTextSizeSlider, mkDownloadButton, mkTemplate, mkTemplateContext, redraw)

templateDimensions :: Dimensions
templateDimensions = { width: 2.0 * 1080.0, height: 2.0 * 1080.0 * 5.0/4.0 }

main :: Effect Unit
main = void $ unsafePartial do
  Just templateContext <- mkTemplateContext "canvas" templateDimensions
  canvasContext <- Canvas.getContext2D templateContext.canvas

  guillotine <- mkImageLayer
    "./img/guillotine.svg"
    { x: 2.0*60.0, y: 2.0*60.0 }
    { scaleX: 2.0, scaleY: 2.0 }
    Canvas.SourceOver

  logo <- mkImageLayer
    "./img/jacobinlogo.svg"
    { x: templateDimensions.width - 2.0*120.0, y: templateDimensions.height - 2.0*200.0 }
    { scaleX: 2.0, scaleY: 2.0 }
    Canvas.SourceOver

  bodyTextLayer <- mkRefLayer $ TextLayer
      { text: "Broodtekst"
      , lineHeight: 0.95
      , position: { x: 120.0, y: 300.0 }
      , fillStyle: "#f00"
      , fontName: "Oswald"
      , fontStyle: "normal"
      , fontWeight: "500"
      , fontSize: 180.0
      , align: AlignLeft
      , baseline: BaselineTop
      , letterSpacing: "-3px"
      , dragOffset: Nothing
      , maxWidth: Just $ templateDimensions.width - 2.0*120.0
      , context: canvasContext
      }
  connectTextAreaPure templateContext "bodytext" bodyTextLayer setText
  connectTextSizeSlider templateContext "bodytext-size" bodyTextLayer

  authorLayer <- mkRefLayer $ TextLayer
      { text: "AUTEUR"
      , lineHeight: 0.95
      , position: { x: 2.0*60.0, y: templateDimensions.height - 2.0*200.0 }
      , fillStyle: "#f00"
      , fontName: "Oswald"
      , fontStyle: "normal"
      , fontWeight: "500"
      , fontSize: 2.0 * 50.0
      , align: AlignLeft
      , baseline: BaselineTop
      , letterSpacing: "-3px"
      , dragOffset: Nothing
      , maxWidth: Nothing
      , context: canvasContext
      }

  connectInputPure templateContext "author" authorLayer (setText <<< toUpper)

  titleLayer <- mkRefLayer $ TextLayer
    { text: "Titel"
    , lineHeight: 0.9
    , position: { x: 2.0*60.0, y: templateDimensions.height - 2.0*200.0 + 2.0*50.0 }
    , maxWidth: Just $ templateDimensions.width - 4.0*2.0*60.0
    , fillStyle: "#f00"
    , fontName: "Oswald"
    , fontStyle: "normal"
    , fontWeight: "700"
    , fontSize: 2.0 * 50.0
    , align: AlignLeft
    , baseline: BaselineTop
    , letterSpacing: "-2px"
    , dragOffset: Nothing
    , context: canvasContext
    }
  connectTextAreaPure templateContext "title" titleLayer setText

  let layers = mkUndraggable @Effect $ mkLayers @Effect
        [ mkSomeLayer guillotine
        , mkSomeLayer logo
        , mkSomeLayer authorLayer
        , mkSomeLayer titleLayer
        , mkSomeLayer bodyTextLayer
        , mkSomeLayer $ mkRectangleLayer { x: 0.0, y: 0.0, width: templateDimensions.width, height: templateDimensions.height } "#fff"
        ]

  template <- mkTemplate templateContext layers
  redraw template
  addEventListeners template
  mkDownloadButton "button" "jacobin.png" template
