// MINESWEEPER
open System

// configuration
let height = 9

let width = 9

let mines = 10

// random function for later
let rng = System.Random()

// cell type
type Cell = {
    Mine: bool
    Revealed: bool
    Adjacent: int
    Flagged: bool
} 

// create an empty board
let createBoard() : Cell[][] = 
    Array.init height (fun _ ->
        Array.init width (fun _ -> 
           { Mine = false; Revealed = false; Adjacent = 0; Flagged = false }))

// place random mines
let placeMines (board: Cell[][]) =
    [|0 .. width * height - 1|]
    |> Array.sortBy (fun _ -> rng.Next())
    |> Array.take mines
    |> Array.iter (fun i ->
        let y = i / width
        let x = i % height
        board[y][x] <- { board[y][x] with Mine = true})
    board

// for marking both mines and adjacent cells
let directions =
        [(-1, -1); (0, -1); (1, -1);
        (-1, 0);         (1, 0);
        (-1, 1); (0, 1); (1, 1);]

// mark adjacent cells
// mark directions/surrounding positions
let countAdjacent (board: Cell[][]) x y =
    directions
    |> List.sumBy (fun (a, b) ->
    let nx = a + x
    let ny = b + y
    if nx >= 0 && nx < width && ny >= 0 && ny < height then
        let cell = board[ny][nx]
        if cell.Mine then 1 else 0
    else 0)


let placeAdjacencyNumbers (board: Cell[][]) =
    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            let cell = board[y][x]
            if not cell.Mine then
                let count = countAdjacent board x y 
                board[y][x] <- { cell with Adjacent = count }
    board


// printing the board
let printBoard (board: Cell[][]) showAll =
    Console.Clear()
    printf "  "
    for x in 0 .. width - 1 do printf "%d " x
    printfn ""
    for y in 0 .. height - 1 do 
        printf "%d " y
        for x in 0 .. width - 1 do
            let cell = board[y][x]
            match cell.Revealed || showAll, cell.Mine, cell.Flagged with
            | true, true, _ -> printf "* "
            | true, false, _ -> printf "%d " cell.Adjacent
            | false, _, true -> printf "F "
            | false, _, _ -> printf "# "  
        printfn ""


// reveal / flag a cell
let cellReveal (board: Cell[][]) x y =
    let cell = board[y][x]
    board[y][x] <- { cell with Revealed = true }


let cellFlagged (board: Cell[][]) x y =
    let cell = board[y][x]
    board[y][x] <- { cell with Flagged = not cell.Flagged }


// flood reveal function
let rec reveal (board: Cell[][]) x y =
    let cell = board[y][x]
    if not cell.Revealed && not cell.Mine then
        cellReveal board x y
        if cell.Adjacent = 0 then
            for (a, b) in directions do
                let nx = x + a
                let ny = y + b
                if nx >= 0 && nx < width && ny >= 0 && ny < height then
                    reveal board nx ny

// win condition
let hasWon (board: Cell[][]) = 
    board
    |> Array.forall (Array.forall (fun cell -> cell.Revealed || cell.Mine ))


// game loop
let rec gameLoop (board: Cell[][]) =
    printBoard board false

    printf "Enter command r/f x y"

    match Console.ReadLine().Split(' ', StringSplitOptions.RemoveEmptyEntries) with
    | [| cmd; sx; sy |] ->

        match Int32.TryParse sx, Int32.TryParse sy with
        | (true, x), (true, y)
            when x >= 0 && x < width && y >= 0 && y < height ->
                let cell = board[y][x]
                
                match cmd with
                | "f" ->
                    cellFlagged board x y |> ignore
                    gameLoop board
                | "r" ->
                    if cell.Mine then
                        printBoard board true
                        printfn "Game Over!!"
                        Console.ReadKey() |> ignore
                    else 
                        reveal board x y
                        match hasWon board with
                        | true -> 
                            printBoard board true
                            printfn "You win!!"
                            Console.ReadKey() |> ignore
                        | false ->
                            gameLoop board
                | _ -> gameLoop board
            | _ -> gameLoop board
    | [|_; _; _; _|] -> gameLoop board
    | _ -> gameLoop board


// starting a game
let board =
    createBoard ()
    |> placeMines
    |> placeAdjacencyNumbers

gameLoop board