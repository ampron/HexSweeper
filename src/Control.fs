module Control

open AppModel
open GameModel
open HexSweeper.Collections

// Reference for Javascript keycodes
// http://gcctech.org/csc/javascript/javascript_keycodes.htm

let keyNoOp st = st

let devKeyMap = [
  // 'c'
  (67 , (fun model -> { model with game= togglePeek model.game }))
]

let initialKeyMap =
  lazy
    let mutable handlers = Array.init 256 (fun _ -> keyNoOp)
    let devKeyCode = 192
    handlers[devKeyCode] <- (fun model ->
      { model with developerModeEnabled= not model.developerModeEnabled }
    )
    handlers[67] <-
      (fun model -> { model with game= togglePeek model.game })
    handlers