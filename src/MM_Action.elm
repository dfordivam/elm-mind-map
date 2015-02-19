module MM_Action where

import Signal

type Action
    = NoOp
    | SelectNode Int
    | AddNode

mm_channel : Signal.Channel Action
mm_channel = Signal.channel NoOp

