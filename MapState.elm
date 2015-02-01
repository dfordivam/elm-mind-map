module MapState where

import MM_Node (..)
import RenderNode (..)
---- Model -----
-- Mind Map is a tree. The node can have multiple children
-- 

type alias State = 
    {  rootNode  : MM_Node
    ,  editNode  : MM_Node
    ,  selectedNodes : List Int
    ,  nodes     : List MM_Node
    ,  uid       : Int
    ,  rootRNode : RenderNode
    }


emptyState : State
emptyState = 
    let root = MM_RootNode { nodeName = "", childNodes = [], text = "root" }
    in { rootNode = root, editNode = root, selectedNodes = [0], nodes = [root], uid = 0, rootRNode = renderMM_Node root}
