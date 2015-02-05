module MM_Tree where

import List (..)

-- The Mind Map Node's structure is a tree representation
-- Its heirarchy is represented by this minimal data structure
-- Each MM_Node has one-to-one mapping with a MM_Tree node.

type MM_Tree = MM_Tree
    {  id           : Int   -- Same as id of MM_Node
    ,  childNodes   : List MM_Tree
    }

getNodeID (MM_Tree n) = n.id
getChildNodes (MM_Tree n) = n.childNodes
getTreeNodeWithId id n = n

-- How to add to tree
-- 1. Create new empty node
-- 2. Get a list of all parent nodes till root
-- 3. update all the parent nodes (starting from root) with new nodes 
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
addNode : MM_Tree -> MM_Tree -> Int -> MM_Tree
addNode r n j =
    let newN = MM_Tree { id = j, childNodes = [] }
        parents = reverse (findAllParents (getNodeID n) r)
        c = getChildNodes n
        add_node_new = MM_Tree { id = getNodeID n, childNodes = (newN :: c) }
    in (updateParentNodes add_node_new parents)

-- Takes an input "new" node (which is part of new tree)
-- Create a new parent node by replacing old node with "new" node
-- recursively call the update on parent node till root
-- This API will return a new root node for new tree
-- InputNode -> ParentNodes -> NewRoot
updateParentNodes : MM_Tree -> List MM_Tree -> MM_Tree
updateParentNodes newN parentList =
    if parentList == [] then newN else
       let p = head parentList
           c = getChildNodes p
       in updateParentNodes (MM_Tree { id = (getNodeID p), childNodes = newN :: c}) (tail parentList)
       --in updateParentNodes newN (tail parentList)



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
    if (id == getNodeID r) 
       then [] 
       else let foundNode = isChild id r
            in if foundNode 
                then r :: (concat (map (findAllParents id) (getChildNodes r)))
                else []
