module MM_StateMock where

import Array (..)

import MM_State (..)
import MM_Node (..)
import MM_Tree (..)
import RenderNode (..)

twoNodes : MM_State
twoNodes = addNode emptyState "Node1" 0

fourChild : MM_State
fourChild = 
    let r1 = addNode emptyState "Node1" 0
        r2 = addNode r1 "Node2" 0
        r3 = addNode r2 "Node3" 0
        r4 = addNode r3 "Node4" 0
    in r4
    
