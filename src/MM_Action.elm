module MM_Action where

import Signal
import Graphics.Input.Field as Field
import Keyboard

type Action
    = NoOp
    | AddNode
    | SelectNode Int
    | EditNode
    | TextField Field.Content
    | EditNodeName String

-- Events from mind map
mm_channel : Signal.Channel Action
mm_channel = Signal.channel NoOp

-- Events from the primary text field.
textField : Signal.Channel Field.Content
textField = Signal.channel Field.noContent

textToAction : Signal Action
textToAction = Signal.map (\x -> TextField x) (Signal.subscribe textField)

getEnterAction : Signal Action
getEnterAction = 
    let text = Signal.map (.string) (Signal.subscribe textField)
        action = Signal.map (\x -> EditNodeName x) text
    in Signal.sampleOn entered action

{-| Signal that updates when the enter key is pressed. We will use it to sample
other signals. Actual value of this signal is not important.
-}
entered : Signal ()
entered = Signal.map (\x -> ()) (Signal.keepIf identity True Keyboard.enter )
