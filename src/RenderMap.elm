module RenderMap where

import Graphics.Input
import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color (..)
import Signal
import Text
import Debug

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
    let renderChildSubTree : List MM_Tree -> Direction -> (Element, (Int, Int))
        renderChildSubTree children dir = 
            let childRenderTree = map2 recursiveRenderNode children (repeat (length children) dir)

                heights  = map (\(_, (_,x)) -> x) childRenderTree
                widths   = map (\(_, (x,_)) -> x) childRenderTree
                totalH   = (if heights == [] then 0 else sum heights)
                totalW   = (if heights == [] then 0 else maximum widths )

                cummHeight : Int -> List Int -> List Int
                cummHeight s l = if l == [] then [] else s :: (cummHeight (s + head l) (tail l)) 

                offsetY  = map2 makeOffsetY heights (cummHeight 0 heights)
                makeOffsetY a b = ((toFloat a) / 2 + (toFloat b) - (toFloat totalH)/2 )

                offsetX  = map makeOffsetX widths
                makeOffsetX a = 
                    let val = ((toFloat totalW) - (toFloat a))/2
                    in if (dir == left) then val else -val

                subTrees = map (\(x, (_,_)) -> toForm x) childRenderTree
                xShifted = map2 moveX offsetX subTrees 
                yShifted = map2 moveY offsetY xShifted

                subTreeOutline = outlined (dashed green) (rect ((toFloat totalW) - 5) ((toFloat totalH) - 5))

                fullSubTree = subTreeOutline :: yShifted
            in  (collage totalW totalH fullSubTree, (totalW, totalH))

        recursiveRenderNode : MM_Tree -> Direction -> (Element, (Int, Int))
        recursiveRenderNode tree dir = 
            let (childSubTree, (w1, h1)) = renderChildSubTree (getChildNodes tree) dir
                node = getNodeWithId (getNodeId tree) state
                rNode = getRenderNodeWithId (getNodeId tree) state
                half_w1 = (toFloat newW)/2
                rNodeShift = (if w1 == 0 then 0 else (if dir == left then half_w1 - 60 else 60 - half_w1))
                subTreeShift = (if dir == left then -60 else 60)
                full = [moveX subTreeShift (toForm childSubTree),
                        moveX rNodeShift rNode.form]
                newH = if h1 > 60 then h1 else 60
                newW = w1 + 120
            in (collage newW newH full, (newW, newH))

        (mmTree, (_,_)) = recursiveRenderNode state.rootNode left

    in mmTree

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
