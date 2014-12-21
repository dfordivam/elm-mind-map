module MindMap where

import Debug
import Graphics.Input as Input
import Maybe

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
newMM_Node : String -> Int-> MM_Node
newMM_Node val i = MM_Node
    {  nodeName   = ""
    ,  childNodes = []
    ,  text       = val
    ,  collapsed  = False
    ,  id         = i
    }

emptyState : State
emptyState = 
    let root = MM_RootNode { nodeName = "", childNodes = [], text = "root", collapsed = False }
    in { rootNode = root, editNode = root , nodes = [root], uid = 0}

getNodeID : MM_Node -> Int
getNodeID n = 
    case n of
      (MM_Node node) -> node.id
      (MM_RootNode node) -> 0

getChildNodes : MM_Node -> [MM_Node]
getChildNodes n = 
    case n of
      (MM_Node node) -> node.childNodes
      (MM_RootNode node) -> node.childNodes
-- 
replaceNode : Int -> MM_Node -> [MM_Node] -> [MM_Node]
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
getNodeWithId : Int -> [MM_Node] -> Maybe MM_Node
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
findAllParents : Int -> MM_Node -> [MM_Node]
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
updateNode : MM_Node -> [MM_Node] -> MM_Node
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

getAllNodes : MM_Node -> [MM_Node]
getAllNodes n = n :: concat (map getAllNodes (getChildNodes n))

testState : State
testState = emptyState
--    let n1 = addNode emptyS.rootNode "n1"
--        n2 = addNode n1 "n2"
--    in { emptyS | rootNode <- n2 }


---- View ----
-- There is a main frame to draw the whole map (It is big collage)
-- The whole map is a big Form which can be moved around
-- each subtree is Form group which is positioned inside a sub-collage
--

view : State -> Element
view state = renderNode state.rootNode right

renderNodeTxt txt id = (color grey (container 100 50 middle (plainText (txt ++ (show id)))) |> Input.clickable actions.handle (AddNode id))

renderOneNode : MM_Node -> Int -> Element
renderOneNode n height = 
    case n of 
      (MM_Node node) ->
        size 100 height (container 100 height middle (renderNodeTxt node.text node.id))
      (MM_RootNode node) ->
        size 100 height (container 100 height middle (renderNodeTxt node.text 0))

evenAndOdd : Bool -> [a] -> ([a], [a])
evenAndOdd right list = 
    if list == [] then ([],[]) 
    else let n = head list
             (l, r) = evenAndOdd (not right) (tail list)
         in if right then (l, n :: r) else (n :: l, r)

renderChildSubtree : [MM_Node] -> Direction -> Element
renderChildSubtree nodes dir = 
        let childNodeMap = map ((flip renderNode) dir) nodes
            childMap = ( intersperse (size 100 30 (spacer 100 30)) childNodeMap)
            height0 = map heightOf childMap
            height1 = if isEmpty height0 then 50 else sum height0
            width0  = map widthOf  childMap
            width1  = if isEmpty width0 then 0 else maximum width0
        in  container width1 height1 middle (flow down childMap)


-- How to create a sub-tree
-- 1. Find the height of child nodes
-- 2. create a container with that height and place root in middle
-- 3. place the child subtrees on the right/left of this container
renderNode : MM_Node -> Direction -> Element
renderNode n dir =
    case n of 
      (MM_Node node) ->
        let childCont = renderChildSubtree node.childNodes dir
            newCont = container (widthOf childCont + 150) (heightOf childCont) middle (flow dir [renderOneNode n (heightOf childCont), size 50 50 (spacer 50 50), childCont])
        in newCont
      (MM_RootNode node) ->
        let (l, r)    = evenAndOdd True node.childNodes
            childContL = renderChildSubtree l left
            childContR = renderChildSubtree r right
            width = widthOf childContL + widthOf childContR + 150
            height = maximum [100, heightOf childContL, heightOf childContR]
            newCont = container width height middle (flow right [childContL, size 50 50 (spacer 50 50), renderOneNode n height, size 50 50 (spacer 50 50), childContR])
        in newCont
--        let childNodeMap = zipWith renderNode node.childNodes directions
--            directions = [right, left, right, left, right]
--            (l, r)    = evenAndOdd True childNodeMap
--            childMapL = flow down ( intersperse (spacer 50 30) l)
--            childMapR = flow down ( intersperse (spacer 50 30) r)
--        in flow right [childMapL, (spacer 50 50 ), (renderOneNode n 100), (spacer 50 50), childMapR]
        
---- Update -----

data Action
    = NoOp
    | AddNode Int

step : Action -> State -> State
step action state = 
    case Debug.watch "Current action : " action of
      NoOp -> state
      
      AddNode id ->
        let node = Maybe.maybe state.rootNode (\x -> x) (getNodeWithId id state.nodes)
            updatedNodeList = (getAllNodes updatedRoot)
            (updatedRoot, newNode) = addNode state.rootNode node "child" (state.uid + 1)
        in { state | uid <- (state.uid + 1), rootNode <- updatedRoot, editNode <- newNode, nodes <- updatedNodeList }
        
      
