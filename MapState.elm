module MapState where

import MM_Node (..)

---- Model -----
-- Mind Map is a tree. The node can have multiple children
-- 

type alias State = 
    {  rootNode  : MM_Node
    ,  editNode  : MM_Node
    ,  selectedNodes : List Int
    ,  nodes     : List MM_Node
    ,  uid       : Int
    }
