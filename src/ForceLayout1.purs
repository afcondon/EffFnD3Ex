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
import Graphics.D3.Scale
import Graphics.D3.Selection
import Graphics.D3.Util
import Prelude(Unit(), bind, negate, (++), show, (>>=))
import Data.Nullable


mainD3 :: forall eff. Eff (d3 :: D3 | eff) Unit
mainD3 = do
  let canvasWidth = 960.0
      canvasHeight = 500.0

  force <- forceLayout
    .. charge (-400.0)
    .. size { width: canvasWidth, height: canvasHeight }
    .. linkDistance 40.0

  drag <- force ... drag
    .. onDragStart customDragStartHandler

  svg <- rootSelect "body"
    .. append "svg"
    .. attr "width" canvasWidth
    .. attr "height" canvasHeight

  json "http://localhost:8080/graph" \(Right v) -> do
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
        .. attr "class" "fixed"
        .. createDrag drag
        -- .. onClick singleClickHandler
        .. onDoubleClick customDoubleClickHandler

    force ... onTick \_ -> do
      link
       ... attrN' "x1" (\d -> d.source.x)
        .. attrN' "y1" (\d -> d.source.y)
        .. attrN' "x2" (\d -> d.target.x)
        .. attrN' "y2" (\d -> d.target.y)

      node
       ... attrN' "cx" _.x
        .. attrN' "cy" _.y

{--}

-- last remaining bits of easy FFI - some helper functions needed for some of this stuff
mySimpleCallback    :: forall eff. Nullable String -> Eff (d3::D3,console::CONSOLE|eff) Unit
mySimpleCallback message = log "mySimpleCallback: Purescript"

-- dragStartHandler    :: forall eff d e r. d -> Eff (d3::D3,console::CONSOLE|eff) (e r)
-- foreign import customDoubleclickHandler :: forall d eff. d -> Eff (d3::D3|eff) Unit

singleClickHandler  :: forall d eff. d -> Eff (d3::D3,console::CONSOLE|eff) Unit
singleClickHandler = ffi ["d"] "d3.select(this).classed('fixed', d.fixed = false);"

foreign import customDragStartHandler   :: forall d e r eff. d -> Eff (d3::D3|eff) (e r)
foreign import customDoubleClickHandler :: forall d eff. d -> Eff (d3::D3|eff) Unit

-- doubleClickHandler  :: forall d i eff. d -> Eff (d3::D3,console::CONSOLE|eff) Unit
-- doubleClickHandler =  ffi ["d"] "d3.selectAll(\".node\").filter(function(d) { return d.index == 10; }).classed('fixed', d.fixed = false);"

-- the graph data tho' should be read with Purescript's Affjax and converted with generics
toGraphData :: Foreign -> GraphData
toGraphData = ffi ["g"] "g"

ffi = unsafeForeignFunction
