#r "../node_modules/fable-core/Fable.Core.dll"

module SameGameTypes =

    type Position = {
        Col:int
        Row:int } with

        member this.Left = { this with Col = this.Col  - 1 }
        member this.Right = { this with Col = this.Col  + 1 }
        member this.Up = { this with Row = this.Row  + 1 }
        member this.Down = { this with Row = this.Row  - 1 }

    type Color = Color of int

    type CellState =
        | Stone of Color
        | Empty

    type Column = CellState list

    type Board = Column list

    type Cell = {
        Position:Position
        State:CellState }

    type Group = {
        Color:Color
        Positions: Position list } 

    type Game = 
        | InProgress of GameState
        | Finished of GameState

    and GameState = {
        Board:Board
        Score:int }

    type GameConfig = {
        NumberOfColumns:int
        NumberOfRows:int
        MaxNumberOfColors:int }

    type SameGameApi = {
        NewGame: (int -> int) -> GameConfig -> Game option
        Play: Game -> Position -> Game }

module SameGameDomain =

    open System
    open SameGameTypes

    let private square x = x * x

    let private bonus = 1000

    let private calcScore groupSize =
        square (groupSize - 2)

    let private penalty stonesLeft =
        -(square (stonesLeft - 2))

    let private getCellState (board:Board) pos =
        let colCount = board |> List.length
        if pos.Col < colCount && pos.Col >= 0 && pos.Row < board.[pos.Col].Length && pos.Row >= 0 then
            board.[pos.Col].[pos.Row]
        else Empty

    let private findAdjacentWithSameColor board col (pos:Position) =
        [pos.Up; pos.Right; pos.Down; pos.Left]
        |> List.map (fun p ->  getCellState board p, p)
        |> List.filter (fun cell -> fst cell = Stone col)
        |> List.map snd

    let private hasValidMoves board = 
        board
        |> Seq.mapi (fun i col -> 
            col 
            |> Seq.mapi (fun j cell -> { Position = { Col = i; Row = j }; State = cell}))
        |> Seq.exists (fun col -> 
            col 
            |> Seq.exists (fun cell -> 
                match cell.State with 
                | Stone c -> cell.Position |> findAdjacentWithSameColor board c  |> (not << List.isEmpty) 
                | _       -> false))

    let private numberOfStones board =
        let numOfStonesInCol = List.sumBy (function Stone c -> 1 | Empty -> 0)
        board |> List.map numOfStonesInCol |> List.sum

    let private isEmpty (board:Board) = board |> List.forall (List.head >> ((=) Empty))

    let private evaluateGameState gameState =
        if gameState.Board |> hasValidMoves then
            InProgress gameState 
        elif gameState.Board |> isEmpty then
            Finished { gameState with Score = gameState.Score + bonus }
        else
            Finished { gameState with Score = gameState.Score + (gameState.Board |> numberOfStones |> penalty) }

    let private getGroup board position =
        let rec find (ps:Position list) col (group:Position list) =
            match ps with
            | []    -> group
            | x::xs -> 
                let cells = x |> findAdjacentWithSameColor board col
                            |> List.filter (fun pos -> not (List.exists ((=) pos) (xs @ group) ))
                find (cells @ xs) col (x :: group)

        getCellState board position
        |> function 
            | Stone c -> 
                let positions = find [position] c []
                if positions |> List.length > 1 then
                    Some { Color = c; Positions = positions }
                else None
            | _ -> None

    let private removeGroup group board =
        board
        |> List.mapi (fun i col -> 
            col 
            |> List.mapi (fun j cell -> { Position = { Col = i; Row = j }; State = cell}) 
            |> List.filter (fun cell -> group.Positions |> (not << List.exists ((=) cell.Position)))
            |> List.map (fun cell -> cell.State)
            |> fun col' -> col' @ List.replicate (col.Length - col'.Length) Empty)
        |> List.filter (List.head >> ((<>) Empty))
        |> fun cols -> cols @ List.replicate (board.Length - cols.Length) (List.replicate (board.[0].Length) Empty)

    let private play gameState pos = 
        getGroup gameState.Board pos
        |> function 
            | Some g -> 
                let newBoard = gameState.Board |> removeGroup g
                { Board = newBoard; Score = gameState.Score + calcScore g.Positions.Length }
            | _ -> gameState

    let private playIfRunning game pos =
        match game with
        | InProgress gameState -> play gameState pos |> evaluateGameState
        | _                    -> game

    let private checkConfig conf onValid onInvalid =
        if conf.MaxNumberOfColors < 3 || conf.MaxNumberOfColors > 8 then
            onInvalid
        elif conf.NumberOfColumns < 3 || conf.NumberOfColumns > 20 then
            onInvalid
        elif conf.NumberOfRows < 3 || conf.NumberOfRows > 12 then
            onInvalid
        else
            onValid conf

    let private newGame rnd config = 
        List.init config.NumberOfColumns (fun _ -> List.init config.NumberOfRows (fun _ -> rnd config.MaxNumberOfColors |> Color |> Stone))
        |> fun board -> { Board = board; Score = 0 }
        |> evaluateGameState |> Some

    let api = {
        NewGame = newGame
        Play = playIfRunning }

open Fable.Core 
open Fable.Import
open System
open SameGameTypes

Node.require.Invoke("core-js") |> ignore

let api = SameGameDomain.api

let play game (x,y) =
    match game with
    | Some g -> 
        Some (api.Play g { Col = x; Row = y })
    | _  -> None

let renderBoard board =
    let renderCell x y col = sprintf "<td class='sg-td'><a href='javaScript:void(0);' id='cell-%d-%d'><div class='sg-cell sg-color%d'></div></a></td>" x y col

    let makeBoard (board: int list list) = 
        "<table class='sg-table horiz-centered'>"
        + String.concat "" [for y in [(board.[0].Length - 1)..(-1)..0] do yield "<tr>" + String.concat "" ([0..(board.Length - 1)] |> List.map (fun x -> renderCell x y board.[x].[y])) + "</tr>"]
        + "</table>"

    makeBoard (board |> List.map (fun col -> col |> List.map (function Stone (Color c) -> c | Empty -> 0)))

let rec updateUi game =
    let boardElement = Browser.document.getElementById("sg-board") :?> Browser.HTMLDivElement
    let scoreElement = Browser.document.getElementById ("sg-score") :?> Browser.HTMLDivElement

    let addListeners maxColIndex maxRowIndex  =
        [0..maxColIndex] |> List.iter (fun x ->
            [0..maxRowIndex] |> List.iter (fun y -> 
                let cellId = sprintf "cell-%d-%d" x y
                let el = Browser.document.getElementById(cellId) :?> Browser.HTMLButtonElement
                el.addEventListener_click((fun _ -> 
                    let game = play game (x,y)
                    updateUi game; null))
                ()))
    
    match game with
    | Some (InProgress gs) -> 
        let board = renderBoard gs.Board
        boardElement.innerHTML <- board
        addListeners (gs.Board.Length - 1) (gs.Board.[0].Length - 1)
        scoreElement.innerText <- sprintf "%i point(s)." gs.Score
    | Some (Finished gs)   -> 
        let board = renderBoard gs.Board
        boardElement.innerHTML <- board
        scoreElement.innerText <- sprintf "No more moves. Your final score is %i point(s)." gs.Score
    | _ -> boardElement.innerText <- "Sorry, an error occurred while rendering the board."

let rnd = new System.Random()
let rndColor i = rnd.Next(i) + 1

let defaultConfig = 
    // no fable support for System.Int32.Parse
    let strToInt (str:string) =
        [for c in str -> c] 
        |> List.rev 
        |> List.mapi (fun i c -> (10.0**(float i)) * (float c)) 
        |> List.sum
        |> int
    
    (Browser.document.getElementById("sg-board") :?> Browser.HTMLDivElement).className
    |> fun className -> className.Split('-') 
    |> Array.map strToInt
    |> fun arr -> { NumberOfColumns =  arr.[0]; NumberOfRows = arr.[1]; MaxNumberOfColors = arr.[2] }

let buttonNewGame = Browser.document.getElementById("new-game") :?> Browser.HTMLButtonElement
buttonNewGame.addEventListener_click((fun _ -> 
    let game = api.NewGame rndColor defaultConfig
    updateUi game; null), false)

api.NewGame rndColor defaultConfig |> updateUi 