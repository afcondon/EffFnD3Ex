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

type GraphData =
  { nodes :: Array Node
  , links :: Array Link
  }

type Node = { x :: Number, y :: Number }
type Link = { source :: Node, target :: Node }

mainD3 :: forall eff. Eff (d3 :: D3 | eff) Unit
mainD3 = do
  let canvasWidth = 960.0
      canvasHeight = 500.0

  force <- forceLayout
    .. size { width: canvasWidth, height: canvasHeight }
    .. charge (-400.0)
    .. linkDistance 40.0

  drag <- force ... drag
    .. onDragStart dragStartHandler

  svg <- rootSelect "body"
    .. append "svg"
    .. attrN "width" canvasWidth
    .. attrN "height" canvasHeight

  json "http://localhost:8080/graph" \(Right v) -> do
    let graph = toGraphData v

    force
     ... nodes graph.nodes
      .. links graph.links
      .. start

    link <- svg ... selectAll ".link"
        .. bindData graph.links
      .. enter .. append "line"
        .. attrS "class" "link"

    node <- svg ... selectAll ".node"
        .. bindData graph.nodes
      .. enter .. append "circle"
        .. attrS "class" "node"
        .. attrN "r" 12.0
        .. createDrag drag
        .. onClick singleClickHandler
        .. onDoubleClick doubleClickHandler

    force ... onTick \_ -> do
      link
       ... attrN' "x1" (\d -> d.source.x)
        .. attrN' "y1" (\d -> d.source.y)
        .. attrN' "x2" (\d -> d.target.x)
        .. attrN' "y2" (\d -> d.target.y)

      node
       ... attrN' "cx" _.x
        .. attrN' "cy" _.y


mySimpleCallback :: forall eff. Nullable String -> Eff (d3 :: D3, console :: CONSOLE | eff) Unit
mySimpleCallback message = log "mySimpleCallback: Purescript"



dragStartHandler :: forall d. d -> D3Eff Unit
dragStartHandler = ffi ["d"] "d3.select(this).classed('unmoored', d.fixed = true);"

singleClickHandler :: forall d eff. d -> Eff (d3 :: D3, console :: CONSOLE | eff) Unit
singleClickHandler d = log "singleClickHandler - Purescript"

doubleClickHandler :: forall d eff. d -> Eff (d3 :: D3, console :: CONSOLE | eff) Unit
doubleClickHandler d = log "doubleClickHandler - Purescript"

-- doubleClickHandler :: forall d. d -> D3Eff Unit
-- doubleClickHandler = ffi ["d"] "d3.select(this).classed('fixed', d.fixed = false);"

toGraphData :: Foreign -> GraphData
toGraphData = ffi ["g"] "g"

ffi = unsafeForeignFunction
