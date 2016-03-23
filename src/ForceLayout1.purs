module Graphics.D3.Examples.ForceLayout1 where

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Either
import Data.Foreign
import Data.Foreign.EasyFFI
import Graphics.D3.Base
import Graphics.D3.Layout.Base
import Graphics.D3.Layout.Force
import Graphics.D3.Request
import Graphics.D3.Event
import Graphics.D3.Scale
import Graphics.D3.Selection
import Graphics.D3.Util
import Graphics.D3.EffFnExtra
import Prelude(Unit(), unit,  bind, negate, (++), show, (>>=), return, ($), liftM1, map)
import Data.Nullable
import Data.Tuple
import Data.Maybe
import Data.Traversable
import DOM
import DOM.Event.Event
import DOM.Event.Types


mainD3 :: forall eff. Eff (d3 :: D3, console :: CONSOLE, dom :: DOM | eff) Unit
mainD3 = do
  let canvasWidth = 960.0
      canvasHeight = 500.0

  force <- forceLayout
    .. charge (-400.0)
    .. size { width: canvasWidth, height: canvasHeight }
    .. linkDistance 40.0

  drag <- force ... drag
    .. onDragStart dragStartHandler

  svg <- rootSelect "body"
    .. append "svg"
    .. attr "width" canvasWidth
    .. attr "height" canvasHeight

  json "http://localhost:3000/graph" \(Right v) -> do
    let graph = toGraphData v

    force
     ... nodes graph.nodes
      .. links graph.links
      .. start

    link <- svg ... selectAll ".link"
        .. bindData graph.links
      .. enter .. append "line"
        .. attr "class" "link"

    node <- svg ... selectAll ".node"
        .. bindData graph.nodes
      .. enter .. append "circle"
        .. attr "class" "node"
        .. attr "r" 12.0
        .. createDrag drag
        .. onClick singleClickHandler
        .. onDoubleClick doubleClickHandler

    force ... onTick \_ -> do
      link
       ... attr' "x1" (\d -> d.source.x)
        .. attr' "y1" (\d -> d.source.y)
        .. attr' "x2" (\d -> d.target.x)
        .. attr' "y2" (\d -> d.target.y)

      node
       ... attr' "cx" _.x
        .. attr' "cy" _.y

mySimpleCallback    :: forall eff. Nullable String -> Eff (d3::D3,console::CONSOLE|eff) Unit
mySimpleCallback message = log "mySimpleCallback: Purescript"


-- Event propagation is a complicated business - see this Stack Overflow when it's time
-- to get this right:
-- http://stackoverflow.com/questions/19931307/d3-differentiate-between-click-and-drag-for-an-element-which-has-a-drag-behavior

dragStartHandler    :: forall eff. Eff (d3::D3,console::CONSOLE,dom::DOM|eff) Unit
dragStartHandler = do
      log "in the dragStartHandler"
      mev <- currentD3Event
      u   <- case mev of
             (Just e) -> stopPropagation e
      return unit


-- | this is just an inline hack to show the idea, in a more comprehensive example the data would be defined
-- | elsewhere and the accessors necessary would be defined there too
type Node = { index :: Number, fixed :: Number, weight :: Number, px :: Number, py :: Number, cx :: Number, cy :: Number }
foreign import setFixed   :: forall eff. Node -> Eff (d3::D3|eff) Unit
foreign import unsetFixed :: forall eff. Node -> Eff (d3::D3|eff) Unit

singleClickHandler  :: forall eff. (ElementAndDatum Node) -> Eff (d3::D3,console::CONSOLE|eff) Unit
singleClickHandler (Tuple d element) = do
        log "in the singleClickHandler"
        setFixed d
        s <- select' element
            .. attr "r" 20.0
            .. classed "fixed" true
        -- mev <- currentD3Event
        -- let prevented = case mev of
        --              (Just e) -> defaultPrevented e
        --              Nothing  -> false
        -- let effect = case prevented of
        --          false -> select' element
        --                   .. attr "r" 20.0
        return unit

doubleClickHandler  :: forall eff. (ElementAndDatum Node) -> Eff (d3::D3,console::CONSOLE|eff) Unit
doubleClickHandler (Tuple d element) = do
        log "in the doubleClickHandler"
        unsetFixed d
        s <- select' element
            .. attr "r" 10.0
        return unit

-- the graph data tho' should be read with Purescript's Affjax and converted with generics
toGraphData :: Foreign -> GraphData
toGraphData = ffi ["g"] "g"

ffi = unsafeForeignFunction
