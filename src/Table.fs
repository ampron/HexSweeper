namespace HexSweeper.Collections


module Array =
  let getItem (i: int) (xs: 't array) : 't option =
    if Array.isEmpty xs then None
    else Array.tryItem i xs

  let tryUpdateAt (i: int) (y: 't) (xs: 't array) : 't array =
    match xs |> getItem i with
    | None -> xs
    | Some(x) ->
      let ys = xs |> Array.copy
      ys.[i] <- y
      ys

  let trySetAt (i: int) (f: 't -> 't) (xs: 't array) : 't array =
    match xs |> getItem i with
    | None -> xs
    | Some(x) ->
      let ys = xs |> Array.copy
      ys.[i] <- f x
      ys


type HexGrid<'t> =
  {
    rows: array<array<option<'t>>>
    sideLen: int
    numCells: int
  }

  member this.Item(rowIdx: int, colIdx: int) : 't option =
    this.rows
    |> Array.getItem rowIdx
    |> Option.map (fun r ->
      if Array.isEmpty r then None
      else
        r |> Array.tryItem colIdx
    )
    |> Option.flatten
    |> Option.flatten


module HexGrid =
  let adjacentIndicies (rowIdx, colIdx) =
    seq{
      yield (rowIdx+1, colIdx)
      yield (rowIdx-1, colIdx)
      yield (rowIdx, colIdx+1)
      yield (rowIdx, colIdx-1)
      yield (rowIdx+1, colIdx-1)
      yield (rowIdx-1, colIdx+1)
    }

  let empty() = { rows= Array.empty; sideLen= 0; numCells= 0 }

  let isEmpty hexes =
    hexes.rows |> Seq.map (fun r -> Array.isEmpty r) |> Seq.reduce (&&)

  let copy (hexes: HexGrid<'t>) : HexGrid<'t> =
    { hexes with
        rows= [| for row in hexes.rows do Array.copy row |]
    }

  let getItem (rowIdx, colIdx) (hexes: HexGrid<'t>) : 't option =
    hexes.rows
    |> Array.getItem rowIdx |> Option.map (Array.getItem colIdx)
    |> Option.flatten |> Option.flatten

  let trySetAt (rowIdx: int, colIdx: int) (f: 't -> 't)  (hexes: HexGrid<'t>) : HexGrid<'t> =
    { hexes with
        rows=
          hexes.rows
          |> Array.trySetAt rowIdx (fun r ->
            r |> Array.trySetAt colIdx (Option.bind (f >> Some))
          )
    }

  let cells (xs: HexGrid<'t>) =
    seq{
      for (rowIdx, row) in Seq.indexed xs.rows do
        for (colIdx, element) in Seq.indexed row do
          match element with
          | None -> ()
          | Some(x) -> yield x
    }

  let indexedCells (xs: HexGrid<'t>) =
    seq{
      for (rowIdx, row) in Seq.indexed xs.rows do
        for (colIdx, element) in Seq.indexed row do
          match element with
          | None -> ()
          | Some(x) -> yield ((rowIdx, colIdx), x)
    }

  let mapi (f: (int * int) -> 't -> 'u) (hexes: HexGrid<'t>) : HexGrid<'u> =
    { rows=
        hexes.rows
        |> Array.mapi (fun rowIdx row ->
          row |> Array.mapi (fun colIdx o ->
            o |> Option.map (fun x -> f (rowIdx, colIdx) x)
          )
        )
      sideLen = hexes.sideLen
      numCells= hexes.numCells
    }


  let rec dfs
    (shouldContinue: 't -> bool)
    (startingCoord: int * int)
    (hexes: HexGrid<'t>)
    : (int * int) seq
    =
      let rec run (gridState: HexGrid<bool * 't>) coordStack =
        match coordStack with
        | (rowIdx, colIdx) :: stackTail ->
          seq{
            match gridState.[rowIdx, colIdx] with
            | None | Some((true, _)) ->
                yield! run gridState stackTail
            | Some((false, x)) ->
              yield (rowIdx, colIdx)
              yield! run (
                gridState |> trySetAt (rowIdx, colIdx) (fun (_, rc) -> (true, rc))
              ) (
                if shouldContinue x then
                  (adjacentIndicies (rowIdx, colIdx) |> Seq.toList) @ stackTail
                else stackTail
              )
          }
        | _ -> Seq.empty
      run (hexes |> mapi (fun _ x -> (false, x))) [startingCoord]


  let trySetMany
    (instructions: seq<(int * int) * ('t -> 't)>)
    (hexes: HexGrid<'t>)
    =
    let outHexes = copy hexes
    for (rowIdx, colIdx), f in instructions do
      match outHexes |> getItem (rowIdx, colIdx) with
      | None -> ()
      | Some(x) ->
        outHexes.rows.[rowIdx].[colIdx] <- Some(f x)
    outHexes

  let init (sideLen: int) (f: int * int -> 't) : HexGrid<'t> =
    if sideLen <= 0 then empty()
    else
      let exclusive n = n - 1
      // memory layout example for size = 3
      //   0\1\2\3\4
      // 0| x x o o o
      // 1|  x o o o o
      // 2|   o o o o o
      // 3|    o o o o x
      // 4|     o o o x x
      let sideLen = max 1 sideLen
      let width = 2 * sideLen - 1
      let mutable numFrontNones = sideLen - 1
      let mutable numFrontSomes = width
      {
        sideLen= sideLen
        numCells= 1 + (Seq.init (sideLen-1) (fun i -> (i+1)*6) |> Seq.sum)
        rows=
          [| for rowIdx in 0 .. exclusive width do
              if rowIdx < sideLen-1 then
                // top half of the rows
                [| for colIdx in 0 .. exclusive width do
                    if colIdx < numFrontNones then
                      None
                    else
                      Some(f (rowIdx, colIdx))
                |]
                numFrontNones <- numFrontNones - 1
              else
                // bottom half of the rows, including the middle row
                [| for colIdx in 0 .. exclusive width do
                    if colIdx < numFrontSomes then
                      Some(f (rowIdx, colIdx))
                    else
                      None
                |]
                numFrontSomes <- numFrontSomes - 1
          |]
      }
