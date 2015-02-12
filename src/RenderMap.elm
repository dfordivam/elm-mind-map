module RenderMap where

import Graphics.Input
import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color (..)
import Signal
import Text

import MM_Node (..)
import MM_State (..)
import RenderNode (..)
import MM_Tree (..)

---- View ----
-- There is a main frame to draw the whole map (It is big collage)
-- The view has a header with controls and the rest is occupied with the map
--
-- Header view
-- Edit Button

-- Mind map
-- 

view : (Int, Int) -> MM_State -> Element
view (w,h) state =
    let mindmap = renderTree state (w,h)
        fullWindow = toForm ( container w h middle (flow down [(spacer 50 50), mindmap]))
    in collage w h [fullWindow]

renderTree : MM_State -> (Int, Int) -> Element
renderTree state (w,h) = 
        -- Render Subtree of all Childrens left/right Adjusted
    let renderChildSubTree : List MM_Tree -> Direction -> (Form, (Float, Float))
        renderChildSubTree children dir = 
            let childRenderTree = map2 recursiveRenderNode children (repeat (length children) dir)

                heights  = map (\(_, (_,x)) -> x) childRenderTree
                widths   = map (\(_, (x,_)) -> x) childRenderTree
                totalH   = sum heights
                totalW   = maximum widths 

                offsetY  = map2 makeOffsetY heights (0 :: heights)
                makeOffsetY a b = (totalH/2) - (a / 2) - b

                offsetX  = map makeOffsetX widths
                makeOffsetX a = (totalW - a)/2

                subTrees = map (\(x, (_,_)) -> x) childRenderTree
                xShifted = map2 moveX offsetX subTrees 
                yShifted = map2 moveY offsetY xShifted

                fullSubTree = group yShifted
            in  (fullSubTree, (totalW, totalH))

        recursiveRenderNode : MM_Tree -> Direction -> (Form, (Float, Float))
        recursiveRenderNode tree dir = 
            let (childSubTree, (w1, h1)) = renderChildSubTree (getChildNodes tree) dir
                node = getNodeWithId (getNodeId tree) state
                rNode = getRenderNodeWithId (getNodeId tree) state
                rNodeShift = if dir == left then (w1/2) + 30 else (w1/2) - 30
                subTreeShift = if dir == left then -60 else 60
                full = group [moveX subTreeShift childSubTree,
                        moveX rNodeShift rNode.form]
            in (full, (((w1 + 60), h1)))

        (mmTree, (_,_)) = recursiveRenderNode state.rootNode right

    in collage w (h - 50) [mmTree]

-- renderNodeTxt txt id = (color grey (container 100 50 middle (Text.plainText (txt ))) )
-- -- |> Graphics.Input.clickable (Signal.send clicks (SelectNode id)))
-- 
-- renderOneNode : MM_Node -> Int -> Element
-- renderOneNode n height = 
--     case n of 
--       (MM_Node node) ->
--         size 100 height (container 100 height middle (renderNodeTxt node.text node.id))
--       (MM_RootNode node) ->
--         size 100 height (container 100 height middle (renderNodeTxt node.text 0))
-- 
-- evenAndOdd : Bool -> List a -> (List a, List a)
-- evenAndOdd right list = 
--     if list == [] then ([],[]) 
--     else let n = head list
--              (l, r) = evenAndOdd (not right) (tail list)
--          in if right then (l, n :: r) else (n :: l, r)
-- 
-- createChildSubtreeContainer : Int -> Position -> Int -> Element -> Element
-- createChildSubtreeContainer w p h e = container w h p e
-- 
-- -- How to create a sub-tree
-- -- 1. Find the height of child nodes
-- -- 2. create a container with that height and place root in middle
-- -- 3. place the child subtrees on the right/left of this container
-- renderChildSubtree : List MM_Node -> Direction -> Element
-- renderChildSubtree nodes dir = 
--         let childNodeMap = map ((flip renderNode) dir) nodes
--             childMap = ( intersperse (size 100 30 (spacer 100 30)) childNodeMap)
--             height0 = map heightOf childMap
--             height1 = if isEmpty height0 then 50 else sum height0
--             width0  = map widthOf childMap
--             width1  = if isEmpty width0 then 0 else maximum width0
--             position = if dir == right then midLeft else midRight
--             containers = map2 (createChildSubtreeContainer width1 position) height0 childMap
--         in  container width1 height1 position (flow down containers)
-- 
-- 
-- renderNode : MM_Node -> Direction -> Element
-- renderNode n dir =
--     case n of 
--       (MM_Node node) ->
--         let childCont = renderChildSubtree node.childNodes dir
--             newCont = container (widthOf childCont + 150) (heightOf childCont) middle (flow dir [renderOneNode n (heightOf childCont), size 50 50 (spacer 50 50), childCont])
--         in newCont
--       (MM_RootNode node) ->
--         let (l, r)    = evenAndOdd True node.childNodes
--             childContL = renderChildSubtree l left
--             childContR = renderChildSubtree r right
--             width = widthOf childContL + widthOf childContR + 150
--             height = maximum [100, heightOf childContL, heightOf childContR]
--             newCont = container width height middle (flow right [childContL, size 50 50 (spacer 50 50), renderOneNode n height, size 50 50 (spacer 50 50), childContR])
--         in newCont
-- 
-- 
