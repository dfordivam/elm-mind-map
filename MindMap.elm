module MindMap where

import Debug
import Maybe
import Signal
import List (..)
import Window
import Graphics.Element (..)

-- My imports
import RenderMap
import MM_Node (..)
import MapState (..)

main : Signal Element
main = Signal.map2 RenderMap.view Window.dimensions state 

-- manage the state of our application over time
state : Signal State
state = Signal.foldp step startingState (Signal.subscribe clicks)

startingState = emptyState

clicks : Signal.Channel Action
clicks = Signal.channel NoOp

---- Update -----

type Action
    = NoOp
    | SelectNode Int
    | AddNode Int

step : Action -> State -> State
step action state = 
    case Debug.watch "Current action : " action of
      NoOp -> state

      SelectNode id ->
        let selectedNode = Maybe.withDefault state.rootNode (getNodeWithId id state.nodes)
        in { state | editNode <- selectedNode, selectedNodes <- [id] }

      AddNode id ->
        let node = Maybe.withDefault state.rootNode (getNodeWithId id state.nodes)
            updatedNodeList = (getAllNodes updatedRoot)
            (updatedRoot, newNode) = addNode state.rootNode node "child" (state.uid + 1)
        in { state | uid <- (state.uid + 1), rootNode <- updatedRoot, editNode <- newNode, nodes <- updatedNodeList }
        
      
