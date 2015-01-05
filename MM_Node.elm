module MM_Node where

type MM_Node = MM_Node 
    {  nodeName   : String
    ,  childNodes : List MM_Node
    ,  text       : String
    ,  collapsed  : Bool
    ,  id         : Int 
    } | MM_RootNode
    { nodeName : String
    ,  childNodes : List MM_Node
    ,  text       : String
    ,  collapsed  : Bool
    } 

