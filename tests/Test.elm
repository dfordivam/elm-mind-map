module MindMap where
import Graphics.Collage (..)
import Color (..)
import Debug

-- Testing simple Collage APIs
-- main = 
--     let myForm1 = filled red (rect 40 20)
--         myForm2 = moveX 40 myForm1
--         myForm3 = filled green (rect 40 20)
--         myForm4 = group [moveY 30 myForm3, myForm3, moveY -30 myForm3]
--         myForm5 = moveX -40 myForm4
--         myForm  = group [myForm5, myForm2]
--     in collage 100 100 [myForm]

import MM_StateMock (..)
import RenderMap

main = RenderMap.view (1000, 1000) (Debug.log "mm" eightChild)
