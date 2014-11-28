import Graphics.Input as Input

main : Signal Element
main = lift view state 
--main = collage 500 500 (fullView)

-- manage the state of our application over time
state : Signal State
state = foldp step startingState actions.signal

startingState = emptyState

-- actions from user input
actions : Input.Input Action
actions = Input.input NoOp

fullView : [Form]
fullView = tree
elseD = [ node "Node1"
  , spacer 30 30
  , node "Node2"
  , childNodes
  ]
 
node txt = color grey (container 100 50 middle (plainText txt))
 
rootNode = toForm (node "root")
 
tree = [rootNode, (moveX 130(toForm childNodes2))]
 
childNodes2 = flow down ( intersperse (spacer 50 30) listNodes )
listNodes = [node "n1", node "n2"]
 
childNodes = collage 200 200 nodes
nodes = [nodeForm "n1", (moveY 70 (nodeForm "n2"))]
nodeForm txt = toForm (node txt)


---- Model -----
-- Mind Map is a tree. The node can have multiple children
-- 

data MM_Node = MM_Node 
    {  nodeName   : String
    ,  childNodes : [MM_Node]
    ,  text       : String
    ,  collapsed  : Bool
    ,  parentNode : MM_Node
    ,  id         : Int 
    } | MM_RootNode
    { nodeName : String
    ,  childNodes : [MM_Node]
    ,  text       : String
    ,  collapsed  : Bool
    }    

type State = 
    {  rootNode  : MM_Node
    ,  editNode  : MM_Node
    ,  nodes     : [MM_Node] 
    ,  uid       : Int
    }

-- Create a new node with given parent
newMM_Node : MM_Node -> String -> Int-> MM_Node
newMM_Node p val i = MM_Node
    {  nodeName   = ""
    ,  childNodes = []
    ,  text       = val
    ,  collapsed  = False
    ,  parentNode = p
    ,  id         = i
    }

emptyState : State
emptyState = 
    let root = MM_RootNode { nodeName = "", childNodes = [], text = "root", collapsed = False }
    in { rootNode = root, editNode = root , nodes = [root], uid = 0}

removeNodeWithId : Int -> [MM_Node] -> [MM_Node]
removeNodeWithId id (n::ns) = 
    let foundNode = 
      case n of
        (MM_Node node) -> node.id == id
        (MM_RootNode node) -> True -- This cannot happen, root cannot be in childList
    in if foundNode then ns else n :: (removeNodeWithId id ns)
 
getNodeWithId : Int -> [MM_Node] -> MM_Node
getNodeWithId id (n::ns) = 
    let foundNode = 
      case n of
        (MM_Node node) -> node.id == id
        (MM_RootNode node) -> True -- This cannot happen, root cannot be in childList
    in if foundNode then n else (getNodeWithId id ns)

-- How to add to tree
-- 1. Create new empty node
-- 2. "add" this node to the target node (ie create a new node from target)
-- 3. update all the parent nodes (till root) with new nodes 
--    (ie create a new tree, with new root and new nodes)

addNode : MM_Node -> String -> Int -> (MM_Node, MM_Node)
addNode n val i = 
    let 
      newN = newMM_Node n val i
      currentNode = case n of
        (MM_Node node) ->
          let updatedChildList = newN :: node.childNodes
          in MM_Node { node | childNodes <- updatedChildList }
        (MM_RootNode node) ->
          let updatedChildList = newN :: node.childNodes
          in MM_RootNode { node | childNodes <- updatedChildList }
    in (updateNode currentNode, newN)

-- Takes an input "new" node (which is part of new tree)
-- Create a new parent node by replacing old node with "new" node
-- recursively call the update on parent node till root
-- This API will return a new root node for new tree
updateNode : MM_Node -> MM_Node
updateNode newN =
    case newN of
      (MM_Node node) -> 
        let newParent = 
          case (node.parentNode) of 
            (MM_Node parent) ->
              let updatedChildList = newN :: (removeNodeWithId node.id parent.childNodes)
              in (MM_Node { parent | childNodes <- updatedChildList})
            (MM_RootNode parent) ->
              let updatedChildList = newN :: (removeNodeWithId node.id parent.childNodes)
              in (MM_RootNode { parent | childNodes <- updatedChildList})            
            
        in updateNode newParent
      (MM_RootNode node) ->
        newN

testState : State
testState = emptyState
--    let n1 = addNode emptyS.rootNode "n1"
--        n2 = addNode n1 "n2"
--    in { emptyS | rootNode <- n2 }


---- View ----

view : State -> Element
view state = renderNode state.rootNode

renderNodeTxt txt id = color grey (container 100 50 middle (plainText (txt ++ (show id)))) |> Input.clickable actions.handle (AddNode id)

renderOneNode : MM_Node -> Element
renderOneNode n = 
    case n of 
      (MM_Node node) ->
        renderNodeTxt node.text node.id
      (MM_RootNode node) ->
        renderNodeTxt node.text 0

renderNode : MM_Node -> Element
renderNode n =
    case n of 
      (MM_Node  node) ->
        let childNodeMap = map renderNode node.childNodes
            childMap = flow down ( intersperse (spacer 50 30) childNodeMap)
        in flow right [(renderOneNode n), (spacer 50 50), childMap]
      (MM_RootNode node) ->
        let childNodeMap = map renderNode node.childNodes
            childMap = flow down ( intersperse (spacer 50 30) childNodeMap)
        in flow right [(renderOneNode n), (spacer 50 50), childMap]
        
---- Update -----

data Action
    = NoOp
    | AddNode Int

step : Action -> State -> State
step action state = 
    case action of
      NoOp -> state
      
      AddNode id ->
        let node = getNodeWithId id state.nodes
            updatedNodeList = newNode :: state.nodes
            (updatedRoot, newNode) = addNode node "child" (state.uid + 1)
        in { state | uid <- (state.uid + 1), rootNode <- updatedRoot, editNode <- newNode, nodes <- updatedNodeList }
        
      