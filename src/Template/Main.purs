module Template.Main
  ( Template'(..)
  , TemplateContext(..)
  , mkTemplateContext
  , Template(..)
  , mkTemplate
  , redraw
  , addEventListeners
  , mkDownloadButton
  , toCanvasCoordinates
  , connectInputPure
  , connectInput
  , connectTextAreaPure
  , connectTextArea
  , connectRange
  , connectRangePure
  , connectTextSizeSlider
  ) where

import Prelude

import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Maybe.Trans.Extra (hoistMaybe)
import Control.Monad.Trans.Class (lift)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Effect (Effect)
import Graphics.Canvas (CanvasElement, Dimensions)
import Graphics.Canvas (canvasToDataURL, clearRect, getCanvasDimensions, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasDimensions, withContext) as Canvas
import Partial.Unsafe (unsafePartial)
import Record as Record
import Template.Layer (Point, mkSomeLayer, class Layer, SomeLayer)
import Template.Layer as Layer
import Template.Layer.Ref (RefLayer, mkRefLayer)
import Template.Layer.Ref as RefLayer
import Template.Layer.Text (TextLayer)
import Template.Layer.Text as TextLayer
import Web.DOM (Element)
import Web.DOM.Document (createElement) as Dom
import Web.DOM.Element (getBoundingClientRect, setAttribute, toEventTarget) as Dom
import Web.DOM.NonElementParentNode (getElementById) as Dom
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener)
import Web.Event.EventTarget as Event
import Web.HTML (HTMLDocument, HTMLElement)
import Web.HTML (window) as Html
import Web.HTML.HTMLDocument (toNonElementParentNode) as Html
import Web.HTML.HTMLDocument as HtmlDocument
import Web.HTML.HTMLElement (click, fromElement, offsetHeight, offsetWidth, toElement) as Html
import Web.HTML.HTMLInputElement as Input
import Web.HTML.HTMLTextAreaElement as TextArea
import Web.HTML.Window (document) as Html
import Web.UIEvent.MouseEvent as MouseEvent

type Template' p =
  { canvas :: CanvasElement
  , canvasElement :: Element
  , canvasHtmlElement :: HTMLElement
  , document :: HTMLDocument
  | p
  }

type TemplateContext = Template' ()

mkTemplateContext :: String -> Dimensions -> Effect (Maybe TemplateContext)
mkTemplateContext canvasId dimensions = runMaybeT do
  canvas <- hoistMaybe =<< lift (Canvas.getCanvasElementById canvasId)
  lift $ Canvas.setCanvasDimensions canvas dimensions

  document <- lift $ Html.document =<< Html.window
  canvasElement <- hoistMaybe =<< lift (Dom.getElementById "canvas" $ Html.toNonElementParentNode document)
  canvasHtmlElement <- hoistMaybe $ Html.fromElement canvasElement

  pure
    { canvas
    , canvasElement
    , canvasHtmlElement
    , document
    }

type Template = Template' (layer :: RefLayer (SomeLayer Effect))

mkTemplate :: forall l. Layer Effect l => TemplateContext -> l -> Effect Template
mkTemplate templateContext layer = do
  refLayer <- mkRefLayer $ mkSomeLayer layer
  pure $ Record.merge templateContext { layer: refLayer }

redraw :: Template -> Effect Unit
redraw template = do
  ctx <- Canvas.getContext2D template.canvas
  { width, height } <- Canvas.getCanvasDimensions template.canvas
  Canvas.clearRect ctx { x: 0.0, y: 0.0, width, height }
  Canvas.withContext ctx $ Layer.draw @Effect ctx template.layer

addEventListeners :: Template -> Effect Unit
addEventListeners template = do
  mousedownListener <- mkMousedownListener template
  mousemoveListener <- mkMousemoveListener template
  mouseupListener <- mkMouseupListener template
  inputListener <- mkInputListener template
  Event.addEventListener (EventType "mousedown") mousedownListener false $ Dom.toEventTarget template.canvasElement
  Event.addEventListener (EventType "mousemove") mousemoveListener false $ Dom.toEventTarget template.canvasElement
  Event.addEventListener (EventType "mouseup") mouseupListener false $ Dom.toEventTarget template.canvasElement
  Event.addEventListener (EventType "input") inputListener false $ HtmlDocument.toEventTarget template.document

mkDownloadButton :: String -> String -> Template -> Effect (Maybe Element)
mkDownloadButton buttonId filename template = runMaybeT do
  let documentNode = Html.toNonElementParentNode template.document
  downloadButtonElement <- hoistMaybe =<< lift (Dom.getElementById buttonId documentNode)
  clickListener <- lift $ Event.eventListener $ \_ -> unsafePartial do
    dataUrl <- Canvas.canvasToDataURL template.canvas
    link <- Dom.createElement "a" $ HtmlDocument.toDocument template.document
    Dom.setAttribute "href" dataUrl link
    Dom.setAttribute "download" filename link
    let Just linkHtmlElement = Html.fromElement link
    Html.click linkHtmlElement
  lift $ Event.addEventListener (EventType "click") clickListener false $ Dom.toEventTarget downloadButtonElement
  pure downloadButtonElement

mkMousedownListener :: Template -> Effect EventListener
mkMousedownListener template = Event.eventListener $ \event -> case MouseEvent.fromEvent event of
    Nothing -> pure unit
    Just mouseEvent -> do
      pos <- Layer.position template.layer
      { x, y } <- toCanvasCoordinates template.canvas template.canvasHtmlElement
        { x: toNumber $ MouseEvent.clientX mouseEvent
        , y: toNumber $ MouseEvent.clientY mouseEvent
        }
      let offset = { offsetX: x - pos.x, offsetY: y - pos.y }
      _ <- Layer.dragStart offset template.layer
      redraw template

mkMousemoveListener :: Template -> Effect EventListener
mkMousemoveListener template = Event.eventListener $ \event -> case MouseEvent.fromEvent event of
    Nothing -> pure unit
    Just mouseEvent -> do
      pos <- Layer.position template.layer
      { x, y } <- toCanvasCoordinates template.canvas template.canvasHtmlElement
        { x: toNumber $ MouseEvent.clientX mouseEvent
        , y: toNumber $ MouseEvent.clientY mouseEvent
        }
      let translation = { translateX: x - pos.x, translateY: y - pos.y }
      _ <- Layer.drag translation template.layer
      redraw template

mkMouseupListener :: Template -> Effect EventListener
mkMouseupListener template = Event.eventListener $ \event -> case MouseEvent.fromEvent event of
    Nothing -> pure unit
    Just mouseEvent -> do
      pos <- Layer.position template.layer
      { x, y } <- toCanvasCoordinates template.canvas template.canvasHtmlElement
        { x: toNumber $ MouseEvent.clientX mouseEvent
        , y: toNumber $ MouseEvent.clientY mouseEvent
        }
      let translation = { translateX: x - pos.x, translateY: y - pos.y }
      _ <- Layer.drag translation template.layer
      _ <- Layer.dragEnd template.layer
      redraw template

mkInputListener :: Template -> Effect EventListener
mkInputListener template = Event.eventListener $ \_ -> redraw template

toCanvasCoordinates :: CanvasElement -> HTMLElement -> Point -> Effect Point
toCanvasCoordinates canvas canvasElement { x, y } = do
  { left, top } <- Dom.getBoundingClientRect $ Html.toElement canvasElement
  offsetWidth <- Html.offsetWidth canvasElement
  offsetHeight <- Html.offsetHeight canvasElement
  canvasWidth <- Canvas.getCanvasWidth canvas
  canvasHeight <- Canvas.getCanvasHeight canvas
  pure
    { x: (x - left) * canvasWidth / offsetWidth
    , y: (y - top) * canvasHeight / offsetHeight
    }

connectInput :: forall l. TemplateContext -> String -> RefLayer l -> (String -> l -> Effect l) -> Effect Unit
connectInput { document } id layer k = unsafePartial do
  Just element <- Dom.getElementById id $ Html.toNonElementParentNode document
  let Just inputElement = Input.fromElement element

  initialValue <- Input.value inputElement
  RefLayer.modifyM_ (k initialValue) layer

  inputListener <- Event.eventListener $ \_ -> do
    value <- Input.value inputElement
    RefLayer.modifyM_ (k value) layer
  Event.addEventListener (EventType "input") inputListener false $ Dom.toEventTarget element

connectInputPure :: forall l. TemplateContext -> String -> RefLayer l -> (String -> l -> l) -> Effect Unit
connectInputPure ctx id layer k = connectInput ctx id layer ((pure <<< _) <<< k)

connectTextArea :: forall l. TemplateContext -> String -> RefLayer l -> (String -> l -> Effect l) -> Effect Unit
connectTextArea { document } id layer k = unsafePartial do
  Just element <- Dom.getElementById id $ Html.toNonElementParentNode document
  let Just inputElement = TextArea.fromElement element

  initialValue <- TextArea.value inputElement
  RefLayer.modifyM_ (k initialValue) layer

  inputListener <- Event.eventListener $ \_ -> do
    value <- TextArea.value inputElement
    RefLayer.modifyM_ (k value) layer
  Event.addEventListener (EventType "input") inputListener false $ Dom.toEventTarget element

connectTextAreaPure :: forall l. TemplateContext -> String -> RefLayer l -> (String -> l -> l) -> Effect Unit
connectTextAreaPure ctx id layer k = connectTextArea ctx id layer ((pure <<< _) <<< k)

connectRange :: forall l. TemplateContext -> String -> RefLayer l -> (Number -> l -> Effect l) -> Effect Unit
connectRange { document } id layer k = unsafePartial do
  Just element <- Dom.getElementById id $ Html.toNonElementParentNode document
  let Just inputElement = Input.fromElement element

  initialValue <- Input.value inputElement
  case Number.fromString initialValue of
    Just value -> RefLayer.modifyM_ (k value) layer
    Nothing -> pure unit

  inputListener <- Event.eventListener $ \_ -> do
    value' <- Input.value inputElement
    case Number.fromString value' of
      Just value -> RefLayer.modifyM_ (k value) layer
      Nothing -> pure unit
  Event.addEventListener (EventType "input") inputListener false $ Dom.toEventTarget element

connectRangePure :: forall l. TemplateContext -> String -> RefLayer l -> (Number -> l -> l) -> Effect Unit
connectRangePure ctx id layer k = connectRange ctx id layer ((pure <<< _) <<< k)

connectTextSizeSlider :: TemplateContext -> String -> RefLayer Effect TextLayer -> Effect Unit
connectTextSizeSlider ctx id layer = connectRangePure ctx id layer TextLayer.setFontSize
