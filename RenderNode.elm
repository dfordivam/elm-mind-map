module RenderNode where

import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color (..)

import MM_Node (..)
-- Tree of rendered nodes

type alias RenderNode =
    {   form        :   Form
    ,   selected    :   Bool
    ,   uid         :   Int
    ,   childNodes  :   List RenderNode
    }

renderMM_Node : MM_Node -> RenderNode
renderMM_Node n = 
    let f = createNodeForm n
        id = getNodeId n
        children = map renderMM_Node (getChildNodes n)
    in {f, False, id, children}
        
renderMindMap : RenderNode -> (Int, Int) -> Element
renderMindMap n (w,h) = collage w (h -50) [n.form]
