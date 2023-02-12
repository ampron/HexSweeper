namespace HexSweeper.Collections


[<StructAttribute>]
type ImArray<'T> =
    val xs: array<'T>
    new(xs: array<'T>) = { xs= xs }

    member this.Item with get(idx) = this.xs.[idx]

module Seq =
    let ofImArray<'T> (array: ImArray<'T>): seq<'T> = array.xs |> Array.toSeq

// based on the F# Array module source
[<RequireQualifiedAccess>]
module ImArray =

    let inline freezeArray<'T> (xs: array<'T>): ImArray<'T> = ImArray(xs)

    let singleton<'T> (item : 'T) : ImArray<'T> =
        item |> Array.singleton |> freezeArray

    let ofSeq source: ImArray<'T> =
        source |> Array.ofSeq |> freezeArray

    let toSeq = Seq.ofImArray

    let updateAt<'T> (index: int) (value: 'T) (source: ImArray<'T>): ImArray<'T> =
        Array.updateAt index value source.xs |> freezeArray

    let mapAt<'T> (index: int) (f: 'T -> 'T) (source: ImArray<'T>): ImArray<'T> =
        let ys = Array.copy source.xs
        ys[index] <- f ys[index]
        ImArray(ys)

    let init count initializer =
        Array.init count initializer |> freezeArray
