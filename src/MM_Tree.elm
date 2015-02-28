module MM_Tree where

import List (..)
import Maybe

-- The Mind Map is a tree
-- Its heirarchy is represented by this minimal data structure
-- Each MM_Node has one-to-one mapping with a MM_Tree node.

type MM_Tree = MM_Tree
    {  id           : Int   -- Same as id of MM_Node
    ,  childNodes   : List MM_Tree
    }

getNodeId (MM_Tree n) = n.id
getChildNodes (MM_Tree n) = n.childNodes

getTreeNodeWithId : Int -> MM_Tree -> MM_Tree
getTreeNodeWithId id n =
    let findNode : MM_Tree -> Maybe MM_Tree
        findNode n1 = 
            let foundList = filter (\x -> (getNodeId x) == id) (n1 :: (getChildNodes n1))
            in if foundList == [] then Maybe.oneOf (map findNode (getChildNodes n1)) else Just (head foundList)
    in Maybe.withDefault n (findNode n)


-- How to add to tree
-- 1. Create new empty node
-- 2. Get a list of all parent nodes till root
-- 3. update all the parent nodes with new child list 
--    (ie create a new tree, with new root and new nodes)
-- For eg. 
-- 'addNode root n4 6' will modify n4, n2 and root; and will return root'
--       
-- +--------------------------------------------------------------+
-- |                                                              |
-- |              root                            root'           |
-- |     +--------+                        +--------+             |
-- |     |        |                        |        |             |
-- |     v        v                        v        v             |
-- |     n1       n2           Add n6      n1       n2'           |
-- |       +------+------+     +------>      +------+------+      |
-- |       |      |      |                   |      |      |      |
-- |       |      |      |                   |      |      |      |
-- |       v      v      v                   v      v      v      |
-- |       n3     n4     n5                  n3     n4'    n5     |
-- |                                                +             |
-- |                                                |             |
-- |                                                |             |
-- |                                                v             |
-- |                                                n6            |
-- |                                                              |
-- +--------------------------------------------------------------+

-- root -> add_node -> new_node_id -> newRoot
addTreeNode : MM_Tree -> MM_Tree -> Int -> MM_Tree
addTreeNode r n j =
    let newN = MM_Tree { id = j, childNodes = [] }
        parents = reverse (findAllParents (getNodeId n) r)
        c = getChildNodes n
        add_node_new = MM_Tree { id = getNodeId n, childNodes = (c ++ [newN]) }
    in (updateParentNodes add_node_new parents)

-- Takes an input "new" node (which is part of new tree)
-- Create a new parent node by replacing old node with "new" node in child list
-- recursively call the update on parent nodes till root
-- This API will return a new root node
-- InputNode -> ParentNodes -> NewRoot
updateParentNodes : MM_Tree -> List MM_Tree -> MM_Tree
updateParentNodes newN parentList =
    if parentList == [] then newN else
       let p = head parentList
           c = filterMap (\x ->  if ((getNodeId x) /= (getNodeId newN)) then Just x else Just newN ) ( getChildNodes p)
       in updateParentNodes 
          (MM_Tree { id = (getNodeId p), childNodes = c}) 
          (tail parentList)


-- DFS search for node
-- Find if 'id' is in child forest of 'r'
isChild : Int -> MM_Tree -> Bool
isChild id (MM_Tree r) = 
    if (r.id == id) || (r.id == 0)
    then True
    else any (isChild id) r.childNodes 

-- Get list of all parent nodes
-- Node ID -> Root Node -> [Root :: All other parents]
findAllParents : Int -> MM_Tree -> List MM_Tree
findAllParents id r = 
    if (id == getNodeId r) 
       then [] 
       else let foundNode = isChild id r
            in if foundNode 
                then r :: (concat (map (findAllParents id) (getChildNodes r)))
                else []
