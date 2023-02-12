module GameModel

open System

open HexSweeper.Collections

module Seq =
  let flatten (seqs: seq<seq<'a>>): seq<'a> =
    seq{
      for xs in seqs do
        for x in xs do
          yield x
    }

module Array =
  let shuffleInPlace (rnd: Random) (xs: 'a array): unit =
    let swapIndicies =
      xs |> Seq.take (xs.Length-1) |> Seq.indexed |> Seq.map fst
    for i in swapIndicies do
      let j = rnd.Next(i, xs.Length)
      let y = xs[i]
      xs[i] <- xs[j]
      xs[j] <- y

// TODO add "undefined" state to enable the first click to be on a 0-cell
type CellStatus =
  | Covered
  | Peek
  | Uncovered
  | Flagged

type Cell =
  {
    containsMine: bool
    numAdjMines: int
    status: CellStatus
  }

let initCell = { containsMine= false; numAdjMines= 0; status= Covered }
let initMineCell = { containsMine= true; numAdjMines= 0; status= Covered }

type GameState =
  {
    rnd: Random
    cellsPerSide: int
    minefield: HexGrid<Cell>
  }


let adjacentIndicies (rowIdx, colIdx) =
  seq{
    yield (rowIdx+1, colIdx)
    yield (rowIdx-1, colIdx)
    yield (rowIdx, colIdx+1)
    yield (rowIdx, colIdx-1)
    yield (rowIdx+1, colIdx-1)
    yield (rowIdx-1, colIdx+1)
  }


let initState (rnd: Random) (sideLen: int) (numMines: int) =
  let numMines = max 0 numMines
  let n = max 1 sideLen
  let emtpyHexes = HexGrid.init n (fun _ -> initCell)
  let cellIndicies =
    emtpyHexes |> HexGrid.indexedCells
    |> Seq.map fst
    |> Seq.toArray
  Array.shuffleInPlace rnd cellIndicies
  let minesToPlace = cellIndicies |> Seq.take numMines
  let minefield =
    emtpyHexes |> HexGrid.trySetMany (seq{
        for (rowIdx, colIdx) in minesToPlace do
          // set mine
          yield ((rowIdx, colIdx), (fun _ -> initMineCell))
          // adjust counts of nearby cells
          for rcAdj in adjacentIndicies (rowIdx, colIdx) do
            yield (rcAdj, (fun c ->
              { c with numAdjMines= c.numAdjMines+1 }
            ))
    })
  // let count = 1 + (Seq.init (n-1) (fun i -> (i+1)*6) |> Seq.sum)
  // let count_ =
  //   minefield |> Table.rows
  //   |> Seq.flatten
  //   |> Seq.map (fun o -> if o.IsSome then 1 else 0)
  //   |> Seq.sum
  // printfn $"{count} == {count_}"

  {
    rnd= rnd
    cellsPerSide = n
    minefield= minefield
  }

let togglePeek game =
  { game with
      minefield=
        game.minefield
        |> HexGrid.mapi (fun _ c ->
          { c with
              status=
                match c.status with
                | Covered -> Peek
                | Peek -> Covered
                | _ -> c.status
          }
        )
  }


let rec autoUncover (rowIdx, colIdx) (minefield: HexGrid<Cell>) : HexGrid<Cell> =
  let coordsToUncover =
    HexGrid.dfs (fun c -> c.numAdjMines = 0 && not c.containsMine) (rowIdx, colIdx) minefield
  minefield |> HexGrid.trySetMany (seq{
    for (rowIdx, colIdx) in coordsToUncover do
      yield ((rowIdx, colIdx), (fun c ->
        if c.status = Covered then { c with status= Uncovered } else c
      ))
  })


let isMineUncovered minefield =
  minefield |> HexGrid.cells
  |> Seq.map (fun c -> c.containsMine && c.status = Uncovered)
  |> Seq.reduce (||)
