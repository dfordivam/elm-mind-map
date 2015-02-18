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
import MM_Node (..)
import MM_State (..)
import MM_Action (..)
import MM_Tree (..)

main : Signal Element
main = Signal.map2 RenderMap.view Window.dimensions state 

-- manage the state of our application over time
state : Signal MM_State
state = Signal.foldp step startingState (Signal.subscribe mm_channel)

startingState = emptyState

---- Update -----

step : Action -> MM_State -> MM_State
step action state = 
    case Debug.watch "Current action : " action of
      NoOp -> state

      SelectNode id ->
        let selectedNode = getNodeWithId id state
        in { state | editNode <- selectedNode, selectedNodes <- [id] }

      AddNode id ->
        let node = getTreeNodeWithId id state.rootNode
            newN = newMM_Node "" newId
            newId = (state.uid + 1)
            updatedRoot = addNode state.rootNode node (state.uid + 1)
        in { state | 
                uid <- newId
           ,    rootNode <- updatedRoot
           ,    editNode <- newN
           ,    nodes <- Array.set newId newN (Array.fromList [])
       }