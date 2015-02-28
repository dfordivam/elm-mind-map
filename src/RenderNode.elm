module RenderNode where

import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color (..)
import Text
import Signal
import Graphics.Input
import Graphics.Input.Field as Field

import MM_Node (..)
import MM_Action (..)

-- Container to store the rendered view of a node.
type alias RenderNode 
    =
    {   form        :   Form
    ,   selected    :   Bool -- Not used
    ,   uid         :   Int
    }

-- Render a clickable static node
createNodeForm : MM_Node -> Form
createNodeForm n = toForm (color grey (container 100 50 middle (Text.plainText (n.nodeName))) 
    |> Graphics.Input.clickable (Signal.send mm_channel (SelectNode n.id)))

-- Render a Text Field
editNodeForm : MM_Node -> Field.Content -> Form
editNodeForm n fieldContent = moveX 50 (toForm (Field.field Field.defaultStyle (Signal.send textField) "" fieldContent))


renderMM_Node : MM_Node -> RenderNode
renderMM_Node n = {
            form = createNodeForm n
       ,    selected = False
       ,    uid = n.id
   }

