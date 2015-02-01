module MM_Action where

import Signal

import MM_Node (..)

type Action
    = NoOp
    | SelectNode Int
    | AddNode Int

mm_channel : Signal.Channel Action
mm_channel = Signal.channel NoOp

