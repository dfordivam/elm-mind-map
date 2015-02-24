module MM_State where

import Array (..)
import Maybe

import MM_Node (..)
import RenderNode (..)
import MM_Tree (..)
---- Model -----
-- Mind Map is a tree. 
-- The individual nodes are represented by MM_Node.
-- MM_Tree captures the heirarchy
-- The two are mapped by id

type alias MM_State = 
    {  rootNode         : MM_Tree
    ,  rootMNode        : MM_Node
    ,  editNode         : Maybe MM_Node
    ,  selectedNodes    : List Int
    ,  nodes            : Array MM_Node
    ,  uid              : Int
    ,  renderNodes      : Array RenderNode
    }


emptyState : MM_State
emptyState = 
    let root = MM_Tree { id = 0, childNodes = []}
        rootN = { nodeName = "Root", text = "", folded = False, id = 0 } 
    in { 
            rootNode = root
       ,    rootMNode = rootN
       ,    editNode = Nothing
       ,    selectedNodes = [0]
       ,    nodes = fromList [rootN]
       ,    uid = 0
       ,    renderNodes = fromList [renderMM_Node rootN]
   }

getNodeWithId : Int -> MM_State -> MM_Node
getNodeWithId id state = Maybe.withDefault state.rootMNode (get id state.nodes)

getRenderNodeWithId : Int -> MM_State -> RenderNode
getRenderNodeWithId id state = Maybe.withDefault (renderMM_Node (getNodeWithId 0 state)) (get id state.renderNodes)
getEditNode : MM_State -> MM_Node
getEditNode state = Maybe.withDefault state.rootMNode state.editNode

addNode : MM_State -> String -> Int -> MM_State
addNode state name id = 
    let newRoot = addTreeNode state.rootNode editNode newId
        newId = state.uid + 1
        newNode = newMM_Node name newId
        editNode = getTreeNodeWithId id state.rootNode
    in { state | rootNode <- newRoot
        , nodes <- push newNode state.nodes
        , uid <- newId
        , renderNodes <- push (renderMM_Node newNode) state.renderNodes        }
