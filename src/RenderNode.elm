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

nodeHeight = 60
nodeWidth = 120

-- Render a clickable static node
createNodeForm : MM_Node -> Form
createNodeForm n = toForm (color grey (container nodeWidth nodeHeight middle (Text.plainText (n.nodeName))) 
    |> Graphics.Input.clickable (Signal.send mm_channel (SelectNode n.id)))

fieldStyle = { highlight = { color = blue, width = 5 }
             , padding =  Field.uniformly 5
            , outline = { color = red, width = Field.uniformly 60, radius = 0 }
            , style = Text.defaultStyle}

-- Render a Text Field
editNodeForm : MM_Node -> Field.Content -> Form
editNodeForm n fieldContent = moveX ((toFloat nodeWidth)/2) (toForm (Field.field fieldStyle (Signal.send textField) "" fieldContent))


renderMM_Node : MM_Node -> RenderNode
renderMM_Node n = {
            form = createNodeForm n
       ,    selected = False
       ,    uid = n.id
   }

