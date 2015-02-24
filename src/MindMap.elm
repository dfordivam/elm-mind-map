module MindMap where

import Debug
import Maybe
import Signal
import List (..)
import Window
import Graphics.Element (..)
import Array

-- My imports
import RenderMap
import RenderNode
import MM_Node (..)
import MM_State (..)
import MM_Action (..)
import MM_Tree (..)

import Graphics.Input.Field as Field

main : Signal Element
main = Signal.map2 RenderMap.view Window.dimensions state

-- manage the state of our application over time
state : Signal (MM_State, Field.Content)
state = Signal.foldp step (startingState, Field.noContent) allSignals

startingState = emptyState

allSignals : Signal Action
allSignals = Signal.mergeMany 
                [ getEnterAction
                , Signal.subscribe mm_channel
                , textToAction
                ]

---- Update -----

step : Action -> (MM_State, Field.Content) -> (MM_State, Field.Content)
step action (state, fc) = 
    case action of
      NoOp -> (state, fc)

      SelectNode id ->
        let selectedNode = getNodeWithId id state
        in ({ state | editNode <- Nothing, selectedNodes <- [id] }
           , fc)

      AddNode -> 
        let newState = addNode state "NewNode" (head state.selectedNodes)
        in (newState, fc)

      EditNode ->
        let newState = {state | editNode <- Just editNode1}
            editNodeId = if state.selectedNodes == [] then 0 else (head state.selectedNodes)
            editNode1 = getNodeWithId editNodeId state
            empty = Field.noContent
            newFc = { empty | string <- editNode1.nodeName }
        in (newState, newFc)

      TextField fieldContent -> (state, fieldContent) 

      EditNodeName name -> 
        let newState = {state | editNode <- Nothing
                       , nodes <- Array.set editNodeId editNode2 state.nodes 
                       , renderNodes <- Array.set editNodeId (RenderNode.renderMM_Node editNode2) state.renderNodes }
            editNodeId = if state.selectedNodes == [] then 0 else (head state.selectedNodes)
            editNode1 = getNodeWithId editNodeId state
            editNode2 = {editNode1 | nodeName <- name}
        in (newState, Field.noContent)
