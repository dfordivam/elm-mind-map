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

type alias RenderNode 
    =
    {   form        :   Form
    ,   selected    :   Bool
    ,   uid         :   Int
    }

createNodeForm : MM_Node -> Form
createNodeForm n = toForm (color grey (container 100 50 middle (Text.plainText (n.nodeName))) 
    |> Graphics.Input.clickable (Signal.send mm_channel (SelectNode (getNodeId n))))

getNodeId n = n.id
getChildNodes _ = []

renderMM_Node : MM_Node -> RenderNode
renderMM_Node n = {
            form = createNodeForm n
       ,    selected = False
       ,    uid = n.id
   }
        
