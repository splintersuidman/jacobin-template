module Jacobin.TitleSlide (main) where

import Prelude

import Data.Foldable (or)
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Graphics.Canvas (Composite(..), getContext2D) as Canvas
import Graphics.Canvas (Dimensions, ScaleTransform, TextAlign(..), TextBaseline(..))
import Jacobin.Layer.OverlayBackground (OverlayBackgroundLayer(..), mkOverlayBackgroundLayer)
import Partial.Unsafe (unsafePartial)
import Template.Layer (class Layer, Point, containsPoint, dragEnd, dragStart, dragTranslateMaybe, draw, mkSomeLayer, position, translate)
import Template.Layer.Group (mkGroup)
import Template.Layer.Image (ImageLayer(..), mkEmptyImageLayer, mkImageLayer)
import Template.Layer.Image as ImageLayer
import Template.Layer.Layers (mkLayers)
import Template.Layer.Rectangle (mkRectangleLayer)
import Template.Layer.Ref (RefLayer, mkRefLayer)
import Template.Layer.Ref as RefLayer
import Template.Layer.Shadow (mkShadow)
import Template.Layer.Shadow as Shadow
import Template.Layer.Snap (mkSnapHorizontal, mkSnapVertical)
import Template.Layer.Text (TextLayer(..))
import Template.Layer.Text as TextLayer
import Template.Layer.Text.Markup (MarkupTextLayer(..))
import Template.Layer.Text.Markup as MarkupTextLayer
import Template.Layer.Undraggable (mkUndraggable, mkUndraggableHorizontal, mkUndraggableVertical)
import Template.Main (addEventListeners, connectCheckboxPure, connectInputPure, connectMarkupTextSizeRange, connectObjectUrlInput, connectScaleRange, connectTextAreaPure, mkDownloadButton, mkDownloadButtonClip, mkTemplate, mkTemplateContext, redraw)

instagramDimensions :: Dimensions
instagramDimensions = { width: 1080.0, height: 1080.0 * 5.0 / 4.0 }

templateResolution :: Number
templateResolution = 2.0

templateResolutionScale :: ScaleTransform
templateResolutionScale = { scaleX: templateResolution, scaleY: templateResolution }

-- Double resolution, two images
templateDimensions :: Dimensions
templateDimensions = { width: 2.0 * templateResolution, height: templateResolution } * instagramDimensions

templateWidth :: Number
templateWidth = templateDimensions.width

templateHeight :: Number
templateHeight = templateDimensions.height

storyDimensions :: Dimensions
storyDimensions = { width: 1080.0, height: 1920.0 }

storyTemplateDimensions :: Dimensions
storyTemplateDimensions = { width: templateResolution, height: templateResolution } * storyDimensions

storyTemplateWidth :: Number
storyTemplateWidth = storyTemplateDimensions.width

storyTemplateHeight :: Number
storyTemplateHeight = storyTemplateDimensions.height

newtype GuillotineLayer = GuillotineLayer
  { guillotine :: ImageLayer
  , xMax :: Number
  }

mkGuillotineLayer :: forall m. MonadEffect m => String -> Point -> ScaleTransform -> Number -> m GuillotineLayer
mkGuillotineLayer path position scale xMax = do
  guillotine <- mkImageLayer path position scale Canvas.SourceOver
  pure $ GuillotineLayer { guillotine, xMax }

instance MonadEffect m => Layer m GuillotineLayer where
  position (GuillotineLayer l) = position l.guillotine
  translate t@{ translateX } (GuillotineLayer l) = do
    x' <- (_ + translateX) <<< (_.x) <$> position l.guillotine
    dimensions <- ImageLayer.dimensions l.guillotine
    let scaleY = (ImageLayer.getScale l.guillotine).scaleY
    let
      guillotine' = case dimensions of
        Just { width } | width /= 0.0 -> ImageLayer.setScale { scaleX: (l.xMax - x') / width, scaleY } l.guillotine
        _ -> l.guillotine
    guillotine <- translate t guillotine'
    pure $ GuillotineLayer l { guillotine = guillotine }

  containsPoint p (GuillotineLayer l) = containsPoint p l.guillotine

  draw ctx (GuillotineLayer l) = draw @m ctx l.guillotine

  dragStart offset (GuillotineLayer l) = do
    guillotine <- dragStart offset l.guillotine
    pure $ GuillotineLayer l { guillotine = guillotine }
  drag translation layer@(GuillotineLayer { guillotine: ImageLayer { dragOffset } }) = dragTranslateMaybe dragOffset translation layer
  dragEnd (GuillotineLayer l) = do
    guillotine <- dragEnd l.guillotine
    pure $ GuillotineLayer l { guillotine = guillotine }

newtype OverlayLayer = OverlayLayer
  { overlayBackground :: OverlayBackgroundLayer
  , guillotine :: GuillotineLayer
  , bodyText :: RefLayer MarkupTextLayer
  , author :: RefLayer TextLayer
  , title :: RefLayer MarkupTextLayer
  }

instance MonadEffect m => Layer m OverlayLayer where
  position (OverlayLayer l) = position l.overlayBackground
  translate translation@{ translateX } (OverlayLayer l) = do
    overlayBackground <- translate translation l.overlayBackground
    guillotine <- translate translation l.guillotine
    author <- translate translation l.author
    title <- translate translation l.title
    RefLayer.modify_ (MarkupTextLayer.mapMaxWidth (_ - translateX)) title
    RefLayer.modify_ (MarkupTextLayer.mapMaxWidth (_ - translateX)) l.bodyText
    pure $ OverlayLayer
      { overlayBackground
      , guillotine
      , author
      , bodyText: l.bodyText
      , title
      }

  containsPoint p (OverlayLayer l) = or <$> traverse (containsPoint p)
    [ mkSomeLayer @m l.overlayBackground
    , mkSomeLayer l.guillotine
    , mkSomeLayer l.bodyText
    , mkSomeLayer l.author
    , mkSomeLayer l.title
    ]

  draw ctx (OverlayLayer l) = void $ traverse (draw @m ctx)
    [ mkSomeLayer @m l.overlayBackground
    , mkSomeLayer l.guillotine
    , mkSomeLayer l.bodyText
    , mkSomeLayer l.author
    , mkSomeLayer l.title
    ]

  dragStart offset (OverlayLayer l) = do
    overlayBackground <- dragStart offset l.overlayBackground
    pure $ OverlayLayer l { overlayBackground = overlayBackground }
  drag translation layer@(OverlayLayer { overlayBackground: OverlayBackgroundLayer { dragOffset } }) = dragTranslateMaybe dragOffset translation layer
  dragEnd (OverlayLayer l) = do
    overlayBackground <- dragEnd l.overlayBackground
    pure $ OverlayLayer l { overlayBackground = overlayBackground }

titleSlide :: Effect Unit
titleSlide = void $ unsafePartial do
  Just templateContext <- mkTemplateContext "canvas-title-slide" templateDimensions
  canvasContext <- Canvas.getContext2D templateContext.canvas

  backgroundImageLayer <- mkRefLayer =<< mkEmptyImageLayer { x: 0.0, y: 0.0 } { scaleX: 1.0, scaleY: 1.0 } Canvas.SourceOver
  connectObjectUrlInput templateContext "image" backgroundImageLayer ImageLayer.loadImage
  connectScaleRange templateContext "image-size" backgroundImageLayer

  guillotine <- mkGuillotineLayer
    "./img/guillotine2x.png"
    { x: templateWidth / 2.0 + 285.0 * templateResolution, y: 60.0 * templateResolution }
    { scaleX: ((templateWidth - 180.0 * templateResolution) - (templateWidth / 2.0 + 285.0 * templateResolution)) / 1560.0, scaleY: 1.0 }
    (templateWidth - 180.0 * templateResolution)

  guillotineWhite <- mkImageLayer
    "./img/guillotinewit2x.png"
    { x: 60.0 * templateResolution, y: 60.0 * templateResolution }
    { scaleX: 1.0, scaleY: 1.0 }
    Canvas.SourceOver

  logoRed <- mkImageLayer
    "./img/jacobinlogo80x200.png"
    { x: templateWidth - (60.0 + 40.0) * templateResolution, y: templateHeight - (60.0 + 100.0) * templateResolution }
    { scaleX: 1.0, scaleY: 1.0 }
    Canvas.SourceOver

  logoWhite <- mkImageLayer
    "./img/jacobinlogowit80x200.png"
    { x: templateWidth / 2.0 - (60.0 + 40.0) * templateResolution, y: templateHeight - (60.0 + 100.0) * templateResolution }
    { scaleX: 1.0, scaleY: 1.0 }
    Canvas.SourceOver

  bodyTextLayer <- mkRefLayer $ MarkupTextLayer
    { text: []
    , lineHeight: 1.1
    , position: { x: templateWidth - templateResolution * 60.0, y: templateResolution * 150.0 }
    , fillStyle: "#f00"
    , font:
        { name: "Oswald"
        , style: { normal: "normal", italic: "italic" }
        , weight: { normal: "500", bold: "700" }
        , size: 90.0 * templateResolution
        }
    , align: AlignRight
    , baseline: BaselineTop
    , letterSpacing: "-3px"
    , emptyLineHeight: 0.25
    , dragOffset: Nothing
    , maxWidth: Just $ templateWidth / 2.0 - 2.0 * 60.0 * templateResolution
    , context: canvasContext
    }
  connectTextAreaPure templateContext "bodytext" bodyTextLayer MarkupTextLayer.setText'
  connectMarkupTextSizeRange templateContext "bodytext-size" bodyTextLayer

  let bigTitleAndAuthorPosition = { x: 60.0 * templateResolution, y: (150.0 + 100.0) * templateResolution }
  bigTitleLayer <- mkRefLayer $ MarkupTextLayer
    { text: []
    , lineHeight: 0.9
    , position: bigTitleAndAuthorPosition
    , maxWidth: Just $ templateWidth / 2.0 - 2.0 * 60.0 * templateResolution
    , fillStyle: "#fff"
    , font:
        { name: "Oswald"
        , style: { normal: "normal", italic: "italic" }
        , weight: { normal: "700", bold: "700" }
        , size: 100.0 * templateResolution
        }
    , align: AlignLeft
    , baseline: BaselineBottom
    , letterSpacing: "-2px"
    , emptyLineHeight: 0.25
    , dragOffset: Nothing
    , context: canvasContext
    }
  connectTextAreaPure templateContext "title-left" bigTitleLayer MarkupTextLayer.setText'
  connectMarkupTextSizeRange templateContext "title-size" bigTitleLayer

  bigAuthorLayer <- mkRefLayer $ TextLayer
    { text: "AUTEUR"
    , lineHeight: 0.95
    , position: bigTitleAndAuthorPosition
    , fillStyle: "#fff"
    , fontName: "Oswald"
    , fontStyle: "normal"
    , fontWeight: "500"
    , fontSize: templateResolution * 50.0
    , align: AlignLeft
    , baseline: BaselineTop
    , letterSpacing: "-3px"
    , dragOffset: Nothing
    , maxWidth: Nothing
    , context: canvasContext
    }
  connectInputPure templateContext "author" bigAuthorLayer TextLayer.setText

  let
    bigTitleAndAuthorLayer = mkSnapVertical bigTitleAndAuthorPosition.y (50.0 * templateResolution)
      $ mkGroup @Effect [ mkSomeLayer bigTitleLayer, mkSomeLayer bigAuthorLayer ]

  smallAuthorLayer <- mkRefLayer $ TextLayer
    { text: "AUTEUR"
    , lineHeight: 0.95
    , position: { x: templateWidth / 2.0 + 128.0 * templateResolution, y: templateHeight - (60.0 + 100.0) * templateResolution }
    , fillStyle: "#f00"
    , fontName: "Oswald"
    , fontStyle: "normal"
    , fontWeight: "400"
    , fontSize: 2.0 * 50.0
    , align: AlignLeft
    , baseline: BaselineBottom
    , letterSpacing: "-3px"
    , dragOffset: Nothing
    , maxWidth: Nothing
    , context: canvasContext
    }
  connectInputPure templateContext "author" smallAuthorLayer (TextLayer.setText <<< toUpper)

  smallTitleLayer <- mkRefLayer $ MarkupTextLayer
    { text: []
    , lineHeight: 0.9
    , position: { x: templateWidth / 2.0 + 128.0 * templateResolution, y: templateHeight - (60.0 + 100.0) * templateResolution }
    , maxWidth: Just $ templateWidth / 2.0 - (128.0 + 2.0 * 60.0 + 20.0) * templateResolution
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
    , emptyLineHeight: 0.25
    , dragOffset: Nothing
    , context: canvasContext
    }
  connectTextAreaPure templateContext "title-right" smallTitleLayer MarkupTextLayer.setText'

  let
    overlayBackgroundLayer = mkOverlayBackgroundLayer
      { x: templateWidth / 2.0 + 60.0 * templateResolution, y: 0.0 }
      { width: templateWidth, height: templateHeight }
      82.5
      "#fff"

  let
    overlayLayer = mkSnapHorizontal (templateWidth / 2.0) (60.0 * templateResolution) $ OverlayLayer
      { overlayBackground: overlayBackgroundLayer
      , guillotine: guillotine
      , bodyText: bodyTextLayer
      , author: smallAuthorLayer
      , title: smallTitleLayer
      }

  let shadowColor = "#555"
  leftSlide <- mkRefLayer
    $ mkShadow { offsetX: 0.0, offsetY: 0.0 } shadowColor 15.0
    $ mkLayers @Effect
        [ mkSomeLayer $ mkUndraggable logoWhite
        , mkSomeLayer $ mkUndraggableHorizontal bigTitleAndAuthorLayer
        , mkSomeLayer $ mkUndraggable guillotineWhite
        ]
  connectCheckboxPure templateContext "shadow" leftSlide \on -> Shadow.setColor $
    if on then shadowColor else "#0000"

  let
    layers = mkLayers @Effect
      [ mkSomeLayer $ mkUndraggable logoRed
      , mkSomeLayer $ mkUndraggableVertical overlayLayer
      , mkSomeLayer leftSlide
      , mkSomeLayer backgroundImageLayer
      , mkSomeLayer $ mkUndraggable $ mkRectangleLayer { x: 0.0, y: 0.0, width: templateWidth, height: templateHeight } "#f00"
      ]

  template <- mkTemplate templateContext layers
  redraw template
  addEventListeners template
  Just _ <- mkDownloadButtonClip "download-left" "jacobin-titel-links.png" { x: 0.0, y: 0.0, width: templateWidth / 2.0, height: templateHeight } template
  Just _ <- mkDownloadButtonClip "download-right" "jacobin-titel-rechts.png" { x: templateWidth / 2.0, y: 0.0, width: templateWidth / 2.0, height: templateHeight } template
  pure unit

story :: Effect Unit
story = void $ unsafePartial do
  Just templateContext <- mkTemplateContext "canvas-story" storyTemplateDimensions
  canvasContext <- Canvas.getContext2D templateContext.canvas

  backgroundImageLayer <- mkRefLayer =<< mkEmptyImageLayer { x: 0.0, y: 0.0 } { scaleX: 1.0, scaleY: 1.0 } Canvas.SourceOver
  connectObjectUrlInput templateContext "image" backgroundImageLayer ImageLayer.loadImage
  connectScaleRange templateContext "image-size-story" backgroundImageLayer

  guillotine <- mkImageLayer
    "./img/guillotinewit2x.png"
    { x: 60.0 * templateResolution, y: 60.0 * templateResolution }
    { scaleX: 1.0, scaleY: 1.0 }
    Canvas.SourceOver

  logo <- mkImageLayer
    "./img/jacobinlogowit80x200.png"
    { x: templateWidth / 2.0 - (60.0 + 40.0) * templateResolution, y: storyTemplateHeight - (60.0 + 100.0) * templateResolution }
    { scaleX: 1.0, scaleY: 1.0 }
    Canvas.SourceOver

  let titleAndAuthorPosition = { x: 60.0 * templateResolution, y: (150.0 + 100.0) * templateResolution }
  titleLayer <- mkRefLayer $ MarkupTextLayer
    { text: []
    , lineHeight: 0.9
    , position: titleAndAuthorPosition
    , maxWidth: Just $ storyTemplateWidth - 2.0 * 60.0 * templateResolution
    , fillStyle: "#fff"
    , font:
        { name: "Oswald"
        , style: { normal: "normal", italic: "italic" }
        , weight: { normal: "700", bold: "700" }
        , size: 100.0 * templateResolution
        }
    , align: AlignLeft
    , baseline: BaselineBottom
    , letterSpacing: "-2px"
    , emptyLineHeight: 0.25
    , dragOffset: Nothing
    , context: canvasContext
    }
  connectTextAreaPure templateContext "title-left" titleLayer MarkupTextLayer.setText'
  connectMarkupTextSizeRange templateContext "title-size-story" titleLayer

  authorLayer <- mkRefLayer $ TextLayer
    { text: "AUTEUR"
    , lineHeight: 0.95
    , position: titleAndAuthorPosition
    , fillStyle: "#fff"
    , fontName: "Oswald"
    , fontStyle: "normal"
    , fontWeight: "500"
    , fontSize: templateResolution * 50.0
    , align: AlignLeft
    , baseline: BaselineTop
    , letterSpacing: "-3px"
    , dragOffset: Nothing
    , maxWidth: Nothing
    , context: canvasContext
    }
  connectInputPure templateContext "author" authorLayer TextLayer.setText

  let
    titleAndAuthorLayer = mkSnapVertical titleAndAuthorPosition.y (50.0 * templateResolution) $ mkGroup @Effect
      [ mkSomeLayer titleLayer
      , mkSomeLayer authorLayer
      ]

  let shadowColor = "#555"
  foregroundElements <- mkRefLayer
    $ mkShadow { offsetX: 0.0, offsetY: 0.0 } shadowColor 15.0
    $ mkLayers @Effect
        [ mkSomeLayer $ mkUndraggable logo
        , mkSomeLayer $ mkUndraggableHorizontal titleAndAuthorLayer
        , mkSomeLayer $ mkUndraggable guillotine
        ]
  connectCheckboxPure templateContext "shadow" foregroundElements \on -> Shadow.setColor $
    if on then shadowColor else "#0000"

  let
    layers = mkLayers @Effect
      [ mkSomeLayer foregroundElements
      , mkSomeLayer backgroundImageLayer
      , mkSomeLayer $ mkUndraggable $ mkRectangleLayer { x: 0.0, y: 0.0, width: storyTemplateWidth, height: storyTemplateHeight } "#f00"
      ]

  template <- mkTemplate templateContext layers
  redraw template
  addEventListeners template
  Just _ <- mkDownloadButton "download-story" "jacobin-story.png" template
  pure unit

main :: Effect Unit
main = titleSlide *> story
