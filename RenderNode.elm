module RenderNode where

import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color (..)
import Text
import Graphics.Input
import Signal

import MM_Node (..)
import MM_Action (..)
-- Tree of rendered nodes

type RenderNode 
    = RenderNode
    {   form        :   Form
    ,   selected    :   Bool
    ,   uid         :   Int
    ,   childNodes  :   List RenderNode
    }

createNodeForm : MM_Node -> Form
createNodeForm n = toForm (color grey (container 100 50 middle (Text.plainText ("Root" ))) |> Graphics.Input.clickable (Signal.send mm_channel (AddNode (getNodeId n))))

getNodeId _ = 0
getChildNodes _ = []

renderMM_Node : MM_Node -> RenderNode
renderMM_Node n = RenderNode {
            form = createNodeForm n
       ,    selected = False
       ,    uid = getNodeId n
       ,    childNodes = map renderMM_Node (getChildNodes n)
   }
        
