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
import Template.Layer.Text (TextLayer(..), setText)
import Template.Layer.Text as TextLayer
import Template.Layer.Undraggable (mkUndraggable, mkUndraggableVertical)
import Template.Main (addEventListeners, connectInputPure, connectObjectUrlInput, connectScaleRange, connectTextAreaPure, connectTextSizeRange, mkDownloadButtonClip, mkTemplate, mkTemplateContext, redraw)

instagramDimensions :: Dimensions
instagramDimensions = { width: 1080.0, height: 1080.0 * 5.0/4.0 }

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

newtype GuillotineLayer = GuillotineLayer
  { guillotine :: ImageLayer
  , xMax :: Number
  }

mkGuillotineLayer :: forall m. MonadEffect m => String -> Point -> ScaleTransform -> Number -> m GuillotineLayer
mkGuillotineLayer path position scale xMax = do
  guillotine <- mkImageLayer path position scale Canvas.SourceOver
  pure $ GuillotineLayer { guillotine , xMax }

instance MonadEffect m => Layer m GuillotineLayer where
  position (GuillotineLayer l) = position l.guillotine
  translate t@{ translateX } (GuillotineLayer l) = do
    x' <- (_ + translateX) <<< (_.x) <$> position l.guillotine
    dimensions <- ImageLayer.dimensions l.guillotine
    let scaleY = (ImageLayer.getScale l.guillotine).scaleY
    let guillotine' = case dimensions of
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
  , bodyText :: RefLayer TextLayer
  , author :: RefLayer TextLayer
  , title :: RefLayer TextLayer
  }

instance MonadEffect m => Layer m OverlayLayer where
  position (OverlayLayer l) = position l.overlayBackground
  translate translation@{ translateX } (OverlayLayer l) = do
    overlayBackground <- translate translation l.overlayBackground
    guillotine <- translate translation l.guillotine
    author <- translate translation l.author
    title <- translate translation l.title
    RefLayer.modify_ (TextLayer.mapMaxWidth (_ - translateX)) title
    RefLayer.modify_ (TextLayer.mapMaxWidth (_ - translateX)) l.bodyText
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

main :: Effect Unit
main = void $ unsafePartial do
  Just templateContext <- mkTemplateContext "canvas" templateDimensions
  canvasContext <- Canvas.getContext2D templateContext.canvas

  backgroundImageLayer <- mkRefLayer =<< mkEmptyImageLayer { x: 0.0, y: 0.0 } { scaleX: 1.0, scaleY: 1.0 } Canvas.SourceOver
  connectObjectUrlInput templateContext "image" backgroundImageLayer ImageLayer.loadImage
  connectScaleRange templateContext "image-size" backgroundImageLayer 

  guillotine <- mkGuillotineLayer 
    "./img/guillotine2x.png"
    { x: templateWidth/2.0 + 285.0 * templateResolution, y: 60.0 * templateResolution }
    { scaleX: ((templateWidth - 180.0 * templateResolution) - (templateWidth/2.0 + 285.0 * templateResolution)) / 1560.0, scaleY: 1.0 }
    (templateWidth - 180.0 * templateResolution)

  logoRed <- mkImageLayer
    "./img/jacobinlogo.svg"
    { x: templateWidth - 120.0 * templateResolution, y: templateHeight - 200.0 * templateResolution }
    templateResolutionScale
    Canvas.SourceOver

  logoWhite <- mkImageLayer
    "./img/jacobinlogowit.svg"
    { x: templateWidth/2.0 - 120.0 * templateResolution, y: templateHeight - 200.0 * templateResolution }
    templateResolutionScale
    Canvas.SourceOver

  bodyTextLayer <- mkRefLayer $ TextLayer
    { text: "Broodtekst"
    , lineHeight: 0.95
    , position: { x: templateWidth - templateResolution * 60.0, y: templateResolution * 150.0 }
    , fillStyle: "#f00"
    , fontName: "Oswald"
    , fontStyle: "normal"
    , fontWeight: "500"
    , fontSize: 180.0
    , align: AlignRight
    , baseline: BaselineTop
    , letterSpacing: "-3px"
    , dragOffset: Nothing
    , maxWidth: Just $ templateWidth/2.0 - 2.0 * 60.0 * templateResolution
    , context: canvasContext
    }
  connectTextAreaPure templateContext "bodytext" bodyTextLayer setText
  connectTextSizeRange templateContext "bodytext-size" bodyTextLayer

  let bigTitleAndAuthorPosition = { x: 60.0 * templateResolution, y: 800.0 * templateResolution }
  bigTitleLayer <- mkRefLayer $ TextLayer
    { text: "Titel"
    , lineHeight: 0.9
    , position: bigTitleAndAuthorPosition
    , maxWidth: Just $ templateWidth/2.0 - 2.0 * 60.0 * templateResolution
    , fillStyle: "#fff"
    , fontName: "Oswald"
    , fontStyle: "normal"
    , fontWeight: "700"
    , fontSize: 2.0 * 100.0
    , align: AlignLeft
    , baseline: BaselineBottom
    , letterSpacing: "-2px"
    , dragOffset: Nothing
    , context: canvasContext
    }
  connectTextAreaPure templateContext "title" bigTitleLayer setText
  connectTextSizeRange templateContext "title-size" bigTitleLayer

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
  connectInputPure templateContext "author" bigAuthorLayer setText

  let bigTitleAndAuthorLayer = mkGroup @Effect
        [ mkSomeLayer bigTitleLayer
        , mkSomeLayer bigAuthorLayer
        ]

  smallAuthorLayer <- mkRefLayer $ TextLayer
    { text: "AUTEUR"
    , lineHeight: 0.95
    , position: { x: templateWidth/2.0 + 128.0 * templateResolution, y: templateHeight - 200.0 * templateResolution }
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
  connectInputPure templateContext "author" smallAuthorLayer (setText <<< toUpper)

  smallTitleLayer <- mkRefLayer $ TextLayer
    { text: "Titel"
    , lineHeight: 0.9
    , position: { x: templateWidth/2.0 + 128.0 * templateResolution, y: templateHeight - 200.0 * templateResolution + 50.0 * templateResolution }
    , maxWidth: Just $ templateWidth/2.0 - (128.0 + 2.0 * 60.0 + 20.0) * templateResolution
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
  connectTextAreaPure templateContext "title" smallTitleLayer setText

  let overlayBackgroundLayer = mkOverlayBackgroundLayer
        { x: templateWidth/2.0 + 60.0 * templateResolution, y: 0.0 }
        { width: templateWidth, height: templateHeight }
        82.5
        "#fff"

  let overlayLayer = OverlayLayer
        { overlayBackground: overlayBackgroundLayer
        , guillotine: guillotine
        , bodyText: bodyTextLayer
        , author: smallAuthorLayer
        , title: smallTitleLayer
        }

  let layers = mkLayers @Effect
        [ mkSomeLayer $ mkUndraggable logoWhite
        , mkSomeLayer $ mkUndraggable logoRed
        , mkSomeLayer bigTitleAndAuthorLayer
        , mkSomeLayer $ mkUndraggableVertical overlayLayer
        , mkSomeLayer backgroundImageLayer 
        , mkSomeLayer $ mkUndraggable $ mkRectangleLayer { x: 0.0, y: 0.0, width: templateDimensions.width, height: templateDimensions.height } "#f00"
        ]

  template <- mkTemplate templateContext layers
  redraw template
  addEventListeners template
  Just _ <- mkDownloadButtonClip "download-left" "jacobin-titel-links.png" { x: 0.0, y: 0.0, width: templateWidth/2.0, height: templateHeight } template
  Just _ <- mkDownloadButtonClip "download-right" "jacobin-titel-rechts.png" { x: templateWidth/2.0, y: 0.0, width: templateWidth/2.0, height: templateHeight } template
  pure unit
