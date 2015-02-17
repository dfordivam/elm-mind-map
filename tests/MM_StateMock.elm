module MM_StateMock where

import Array (..)

import MM_State (..)
import MM_Node (..)
import MM_Tree (..)
import RenderNode (..)

twoNodes : MM_State
twoNodes = 
    let newRoot = addNode emptyState.rootNode emptyState.rootNode 1
        newNode = newMM_Node "node1" 1
    in { emptyState | rootNode <- newRoot
        , nodes <- push newNode emptyState.nodes
        , uid <- 1
        , renderNodes <- push (renderMM_Node newNode) emptyState.renderNodes        }

