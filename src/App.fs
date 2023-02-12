module App

open System

open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React

open HexSweeper.Collections
open AppModel
open GameModel
open View

let numMines = 50
let sideLen = 10

// MODEL
//------------------------------------------------------------------------------
let init() : Model =
  {
    game= initState (Random()) sideLen numMines
    keyMap= Control.initialKeyMap.Value
    developerModeEnabled= false
  }

// reference: https://elmish.github.io/elmish/docs/subscription.html
let keyEvents _initialModel =
  let subscription dispatch =
    Browser.Dom.document.addEventListener("keyup", (fun e ->
      let ke = e :?> Browser.Types.KeyboardEvent
      SingleKeyPress(ke) |> dispatch
    ))

  Cmd.ofSub subscription

// UPDATE
//------------------------------------------------------------------------------
let update (msg: Msg) (model: Model) : Model =
  match msg with
  | SingleKeyPress(ke) ->
    model.keyMap[int(ke.keyCode)] model

  | Restart -> { model with game= initState model.game.rnd sideLen numMines }

  | ToggleFlag(rowIdx, colIdx) ->
    { model with
        game=
          { model.game with
              minefield=
                model.game.minefield
                |> HexGrid.trySetAt (rowIdx, colIdx) (fun c ->
                    { c with
                        status=
                          match c.status with
                          | Covered -> Flagged
                          | Flagged -> Covered
                          | _ -> c.status
                    }
                  // )
                )
    } }

  | Uncover(rowIdx, colIdx) ->
    let minefield = model.game.minefield |> autoUncover (rowIdx, colIdx)
    let outModel = { model with game= { model.game with minefield= minefield } }
    if isMineUncovered outModel.game.minefield then
      { outModel with game= togglePeek outModel.game }
    else
      outModel

  | ClearAround(rowIdx, colIdx) ->
    // TODO add check to ensure the cell is surrounded by the same number of flags
    //      as adjacent mines, otherwise do nothing (to keep the player safe)
    let numAdjFlags =
      adjacentIndicies (rowIdx, colIdx)
      |> Seq.sumBy (fun coords ->
        match model.game.minefield |> HexGrid.getItem coords with
        | None -> 0
        | Some(c) -> if c.status = Flagged then 1 else 0
      )
    let okToClear =
      model.game.minefield |> HexGrid.getItem (rowIdx, colIdx)
      |> Option.map (fun c -> c.numAdjMines = numAdjFlags)
      |> Option.defaultValue false
    if okToClear then
      let mutable minefield = model.game.minefield
      for rcAdj in adjacentIndicies (rowIdx, colIdx) do
        minefield <- minefield |> autoUncover rcAdj
      let outModel = { model with game= { model.game with minefield= minefield }}
      if isMineUncovered outModel.game.minefield then
        { outModel with game= togglePeek outModel.game }
      else
        outModel
    else
      model

let hexGrid (width: float) (margin: int) (gm: GameState) dispatch =
  // ref: https://www.redblobgames.com/grids/hexagons/#basics
  let size = width / (sqrt 3.)
  let height = 2. * size
  let gridSize = gm.cellsPerSide
  let gridWidth = float (gridSize + gridSize - 1) * (width + 2. * float margin)
  let m = float margin
  // let n = float (cells.rows.Length)
  // let width_ = gridWidth / float (2*gridSize-1) - 2. * m
  let gridHeight =
    float (gridSize - 1) * (3. * size) + 2. * size
    + float (gridSize + gridSize - 1) * sqrt 3. * float margin
  div [Style([
    Width($"{gridWidth}px")
    Height($"{gridHeight}px")
    PaddingTop($"{margin}px")
    PaddingBottom($"{margin}px")
  ])] [
    for rowIdx, row in gm.minefield.rows |> Seq.indexed do
      div [Class("hexRow")] [
        for _ in 1..abs (gridSize-1 - rowIdx) do
          div [
            Class("hexSpace")
            Style([
              Width($"{0.5 * width}px")
              Height($"{height}px")
              MarginLeft($"%d{margin}px")
              MarginBottom($"{sqrt 3. * float margin - 0.5 * size}px")
            ])
          ] []
        for colIdx, cell in row |> Seq.indexed do
          match cell with
          | None -> ()
          | Some(cell) ->
            div [
              Class("hexCell")
              Style([
                Width($"{width}px")
                Height($"{height}px")
                MarginLeft($"%d{margin}px")
                MarginRight($"%d{margin}px")
                MarginBottom($"{sqrt 3. * float margin - 0.5 * size}px")
                Background(
                    match cell.status with
                    | Covered | Flagged -> "rgb(140, 140, 140)"
                    | Uncovered | Peek ->
                      if cell.containsMine then
                        "rgb(180, 75, 75)"
                      else
                        match cell.numAdjMines with
                        | 0 -> "#DCDBD9"
                        | 1 -> "#C5C0AF"
                        | 2 -> "#B9AF8B"
                        | 3 -> "#AE946D"
                        | 4 -> "#A67A60"
                        | 5 -> "#9E6155"
                        | _ -> "#8D4341"
                )
              ])
              match cell.status with
              | Covered ->
                OnClick(fun e ->
                  ToggleFlag(rowIdx, colIdx) |> dispatch
                )
                OnContextMenu(fun e ->
                  e.preventDefault()
                  Uncover(rowIdx, colIdx) |> dispatch
                )
              | Flagged ->
                OnClick(fun e ->
                  // e.preventDefault()
                  ToggleFlag(rowIdx, colIdx) |> dispatch
                )
              | Uncovered ->
                OnDoubleClick(fun e ->
                  ClearAround(rowIdx, colIdx) |> dispatch
                )
              | _ -> ()
            ] [
              match cell.status with
              | Flagged ->
                p [] [str "🚩"]
              | Peek ->
                p [
                  if not cell.containsMine then Style([FontWeight("bold")])
                ] [
                  if cell.containsMine then str $"💣"
                  else if 0 < cell.numAdjMines then str $"%d{cell.numAdjMines}"
                ]
              | Uncovered ->
                p [
                  if not cell.containsMine then Style([FontWeight("bold")])
                ] [
                  if cell.containsMine then str $"💥"
                  else if 0 < cell.numAdjMines then str $"%d{cell.numAdjMines}"
                ]
              | Covered -> ()
            ]
      ]
  ]


// VIEW (rendered with React)
//------------------------------------------------------------------------------
let view (model: Model) dispatch =
  div [] [
    button [
      OnClick(fun _ -> Restart |> dispatch)
    ] [str "restart"]
    p [] [
      let n =
        model.game.minefield |> HexGrid.cells |> Seq.sumBy (fun c ->
          if c.status = Flagged then 1 else 0
        )
      str $"🚩 {n} / {numMines}"
    ]
    hexGrid 50 2 model.game dispatch
  ]

// App
//------------------------------------------------------------------------------
Program.mkSimple init update view
|> Program.withSubscription keyEvents
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
