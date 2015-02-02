module MM_Tree where

-- The Mind Map Node's structure is a tree representation
-- Its heirarchy is represented by this minimal data structure
-- Each MM_Node has one-to-one mapping with a MM_Tree node.

type MM_Tree = MM_Tree
    {  id           : Int   -- Same as id of MM_Node
    ,  childNodes   : List MM_Tree
    }
