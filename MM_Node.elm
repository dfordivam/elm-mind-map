module MM_Node where

import List (..)

-- Schema for node from freeplane
-- <xs:element name='node'>
--  <xs:complexType>
--   <xs:choice minOccurs='0' maxOccurs='unbounded'>
--    <xs:element ref='arrowlink'/>
--    <xs:element ref='cloud'/>
--    <xs:element ref='edge'/>
--    <xs:element ref='font'/>
--    <xs:element ref='hook'/>
--    <xs:element ref='icon'/>
--    <xs:element ref='node'/>
--    <!-- For nodes with extended formatting content or for notes to nodes. -->
--    <xs:element ref='richcontent'/>
--    <xs:element ref='attribute_layout'/>
--    <xs:element ref='attribute'/>
--   </xs:choice>
--   <xs:attribute name='BACKGROUND_COLOR' type='xs:string' use='optional'/>
--   <xs:attribute name='COLOR' type='xs:string' use='optional'/>
--   <xs:attribute name='FOLDED' use='optional'>
--    <xs:simpleType>
--     <xs:restriction base='xs:string'>
--      <xs:enumeration value='true'/>
--      <xs:enumeration value='false'/>
--     </xs:restriction>
--    </xs:simpleType>
--   </xs:attribute>
--   <xs:attribute name='ID' type='xs:ID' use='optional'/>
--   <xs:attribute name='LINK' type='xs:string' use='optional'/>
--   <xs:attribute name='POSITION' use='optional'>
--    <xs:simpleType>
--     <xs:restriction base='xs:string'>
--      <xs:enumeration value='left'/>
--      <xs:enumeration value='right'/>
--     </xs:restriction>
--    </xs:simpleType>
--   </xs:attribute>
--   <xs:attribute name='STYLE' type='xs:string' use='optional'/>
--   <xs:attribute name='TEXT' type='xs:string' use='optional'/>
--   <xs:attribute name='LOCALIZED_TEXT' type='xs:string' use='optional'/>
--   <xs:attribute name='TYPE' type='xs:string' use='optional'/>
--   <xs:attribute name='CREATED' type='xs:integer' use='optional'/>
--   <xs:attribute name='MODIFIED' type='xs:integer' use='optional'/>
--   <xs:attribute name='HGAP' type='xs:integer' use='optional'/>
--   <xs:attribute name='VGAP' type='xs:integer' use='optional'/>
--   <xs:attribute name='VSHIFT' type='xs:integer' use='optional'/>
--   <xs:attribute name='ENCRYPTED_CONTENT' type='xs:string' use='optional'/>
--  </xs:complexType>
-- </xs:element>

type MM_Node = MM_Node 
    {  nodeName   : String
    ,  childNodes : List MM_Node
    ,  text       : String
    ,  folded     : Bool
    ,  id         : Int
--    ,  style      : MM_NodeStyle
    } | MM_RootNode
    {  nodeName   : String
    ,  childNodes : List MM_Node
    ,  text       : String
--    ,  style      : MM_NodeStyle
    } 

-- Some APIs to work with Model
-- Create a new node with given id
newMM_Node : String -> Int-> MM_Node
newMM_Node val i = MM_Node
    {  nodeName   = ""
    ,  childNodes = []
    ,  text       = val
    ,  folded     = False
    ,  id         = i
    }

getNodeID : MM_Node -> Int
getNodeID n = 
    case n of
      (MM_Node node) -> node.id
      (MM_RootNode node) -> 0

getChildNodes : MM_Node -> List MM_Node
getChildNodes n = 
    case n of
      (MM_Node node) -> node.childNodes
      (MM_RootNode node) -> node.childNodes
-- 
replaceNode : Int -> MM_Node -> List MM_Node -> List MM_Node
replaceNode id newN list = 
    if list == []
    then [] 
    else let n = head list
             ns = tail list
             foundNode =          
              case n of
                (MM_Node node) -> node.id == id
                (MM_RootNode node) -> True
         in if foundNode then (newN :: ns) else n :: (replaceNode id newN ns)

-- Linear search in list for node
getNodeWithId : Int -> List MM_Node -> Maybe MM_Node
getNodeWithId id list = 
    if list == [] 
    then Nothing 
    else let n = head list
             ns = tail list
         in if (id == getNodeID n) then Just n else (getNodeWithId id ns)

-- How to add to tree
-- 1. Create new empty node
-- 2. Get a list of all parent nodes till root
-- 3. update all the parent nodes (starting from root) with new nodes 
--    (ie create a new tree, with new root and new nodes)

-- root -> node -> val -> id -> (newRoot, newNode)
addNode : MM_Node -> MM_Node -> String -> Int -> (MM_Node, MM_Node)
addNode r n val i = 
    let 
      newN = newMM_Node val i
      parents = reverse (findAllParents (getNodeID n) r)
      currentNode = case n of
        (MM_Node node) ->
          let updatedChildList = node.childNodes ++ [newN]
          in MM_Node { node | childNodes <- updatedChildList }
        (MM_RootNode node) ->
          let updatedChildList = node.childNodes ++ [newN]
          in MM_RootNode { node | childNodes <- updatedChildList }
    in ( (updateNode currentNode parents), newN)

-- DFS for node
dfsNode : Int -> MM_Node -> Bool
dfsNode id r =
      case r of
        (MM_Node node) ->
            if node.id == id
            then True
            else any (dfsNode id) node.childNodes 
        (MM_RootNode _ ) -> True

-- Get list of all parent nodes
-- Node ID -> Root Node -> [Root :: All other parents]
findAllParents : Int -> MM_Node -> List MM_Node
findAllParents id r = if (id == getNodeID r) then [] else
    let foundNode = dfsNode id r
    in if foundNode 
       then r :: (concat (map (findAllParents id) (getChildNodes r)))
       else []

-- Takes an input "new" node (which is part of new tree)
-- Create a new parent node by replacing old node with "new" node
-- recursively call the update on parent node till root
-- This API will return a new root node for new tree
-- InputNode -> ParentNodes -> NewRoot
updateNode : MM_Node -> List MM_Node -> MM_Node
updateNode newN list =
    if list == [] then newN else
        case newN of
          (MM_Node node) ->
            let newParent = 
              case (head list) of 
                (MM_Node parent) ->
                  let updatedChildList = replaceNode node.id newN parent.childNodes
                  in (MM_Node { parent | childNodes <- updatedChildList})
                (MM_RootNode parent) ->
                  let updatedChildList = replaceNode node.id newN parent.childNodes
                  in (MM_RootNode { parent | childNodes <- updatedChildList})            
              
            in updateNode newParent (tail list)
          (MM_RootNode node) ->
            newN

getAllNodes : MM_Node -> List MM_Node
getAllNodes n = n :: concat (map getAllNodes (getChildNodes n))


-- type MM_NodeStyle = NoStyle
