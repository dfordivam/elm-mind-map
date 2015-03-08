module RenderMap where

import Graphics.Input
import Graphics.Input.Field as Field
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
import MM_Action (..)


---- View ----
-- The view has a header with some controls and the rest is occupied with the map
-- There is a main frame/collage to draw the whole map

view : (Int, Int) -> (MM_State, Field.Content) -> Element
view (w,h) (state, fc) =
    let mindmap = renderTree (state, fc) (w, h - headerHeight)
        fullWindow = toForm ( container w (h - headerHeight) middle (flow down [mindmap]))
    in collage w h [moveY ((toFloat (h - headerHeight))/2) (toForm renderHeader), fullWindow]

headerHeight : Int
headerHeight = 100

nodeSpacingV = 20
nodeSpacingH = 30

nodeShiftH = (toFloat (nodeWidth + nodeSpacingH)) /2
nodeShiftV = (toFloat (nodeHeight + nodeSpacingV)) /2

renderHeader : Element
renderHeader = container 200 headerHeight middle (flow right 
        [
            Graphics.Input.button (Signal.send mm_channel AddNode) "Add New Node"
        ,   Graphics.Input.button (Signal.send mm_channel EditNode) "Edit Node"
        ])

renderTree : (MM_State, Field.Content) -> (Int, Int) -> Element
renderTree (state, fc) (w,h) = 
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
                makeOffsetY a b = (toFloat totalH)/2 - (toFloat a) / 2 - (toFloat b)

                offsetX  = map makeOffsetX widths
                makeOffsetX a = 
                    let val = ((toFloat totalW) - (toFloat a))/2
                    in if (dir == left) then val else -val

                subTrees = map (\(x, (_,_)) -> toForm x) childRenderTree
                xShifted = map2 moveX offsetX subTrees 
                yShifted = map2 moveY offsetY xShifted

                -- subTreeOutline is for debugging
                -- subTreeOutline = outlined (dashed green) (rect ((toFloat totalW) - 5) ((toFloat totalH) - 5))

                fullSubTree = {- subTreeOutline :: -} yShifted

                -- Create line segments to join parent with children
                segmentStart = 
                    let val = (toFloat totalW + nodeSpacingH)/2
                    in if dir == left then (val,0) else (-val,0)
                segmentEnds = 
                    let val = (toFloat totalW - nodeSpacingH)/2
                        xpos = if dir == left then val else -val
                    in map2 (\x y -> (x,y)) (repeat (length children) xpos) offsetY
                allSegments = map (segment segmentStart) segmentEnds
                lineStyle = {defaultLine | color <- darkGrey} 
                lineS = group (map (traced lineStyle) allSegments)
 
            in  (collage (totalW + nodeSpacingH) totalH (lineS::fullSubTree), (totalW, totalH))

        isEditingNode = state.editNode /= Nothing

        getRenderedNode : MM_Tree -> Form
        getRenderedNode tree = 
            let rNode = getRenderNodeWithId (getNodeId tree) state
                lineStl = {defaultLine | width <- 8, color <- blue}
                outLinedNode = outlined (lineStl) (rect nodeWidth nodeHeight)
                selected = filter (\x -> x == getNodeId tree) state.selectedNodes 
                rendered = if (selected == []) 
                              then rNode.form
                              else group [outLinedNode, rNode.form]

                editingNode = isEditingNode && (getEditNode state).id == (getNodeId tree)
            in if (editingNode) then editNodeForm (getNodeWithId (getNodeId tree) state) fc else rendered


        recursiveRenderNode : MM_Tree -> Direction -> (Element, (Int, Int))
        recursiveRenderNode tree dir = 
            let (childSubTree, (w1, h1)) = renderChildSubTree (getChildNodes tree) dir

                half_w1 = (toFloat newW)/2 - nodeShiftH
                rNodeShift = (if w1 == 0 then 0 else (if dir == left then half_w1 else -half_w1))
                subTreeShift = (if dir == left then -nodeShiftH else nodeShiftH)
                full = [moveX rNodeShift (getRenderedNode tree)
                    , moveX subTreeShift (toForm childSubTree)]

                newH = if h1 > (nodeHeight + nodeSpacingV) then h1 else (nodeHeight + nodeSpacingV)
                newW = w1 + (nodeWidth + nodeSpacingH)
            in (collage newW newH full, (newW, newH))

        renderRootNode : MM_Tree -> Element
        renderRootNode tree = 
            let (leftChildren, rightChildren) = evenAndOdd True (getChildNodes tree)
                (leftSubTree, (lw, lh)) = renderChildSubTree leftChildren left
                (rightSubTree, (rw, rh)) = renderChildSubTree rightChildren right
                totalH = maximum [lh, rh, (nodeHeight + nodeSpacingV) ]
                moreW = if lw > rw then lw else rw
                totalW = 2*moreW + (nodeWidth + nodeSpacingH)

                full = [moveX -((toFloat lw)/2 + nodeShiftH) (toForm leftSubTree)
                       , getRenderedNode tree
                       , moveX ((toFloat rw)/2 + nodeShiftH) (toForm rightSubTree)]

            in  (collage totalW totalH full)

    in renderRootNode state.rootNode

evenAndOdd : Bool -> List a -> (List a, List a)
evenAndOdd right list = 
    if list == [] then ([],[]) 
    else let n = head list
             (l, r) = evenAndOdd (not right) (tail list)
         in if right then (l, n :: r) else (n :: l, r)
