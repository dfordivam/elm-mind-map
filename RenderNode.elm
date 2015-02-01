module RenderNode where

import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color (..)

import MM_Node (..)
-- Tree of rendered nodes

type RenderNode 
    = RenderNode
    {   form        :   Form
    ,   selected    :   Bool
    ,   uid         :   Int
    ,   childNodes  :   List RenderNode
    }

createNodeForm : MM_Node -> Form
createNodeForm n = filled grey (rect 50 100)

getNodeId _ = 0
getChildNodes _ = []

renderMM_Node : MM_Node -> RenderNode
renderMM_Node n = RenderNode {
            form = createNodeForm n
       ,    selected = False
       ,    uid = getNodeId n
       ,    childNodes = map renderMM_Node (getChildNodes n)
   }
        
