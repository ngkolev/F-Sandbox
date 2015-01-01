open System

let random = new Random();

// Structure
type Snake = {
    direction : int * int
    size: int * int
    prize: int * int
    location: (int * int) list
}

type Move = 
    | Up
    | Down
    | Left
    | Right

// Create new snake
let getPrizePosition (width, height) =
    (random.Next(width), random.Next(height))

let initSnake size = {
    direction = (0, 1)
    size = size
    prize = getPrizePosition size
    location = [(fst size / 2, 0)]
};

// Update snake
let getNewHead (snake: Snake) =
    let oldHead = snake.location.Head
    (fst(oldHead) + fst(snake.direction), snd(oldHead) + snd(snake.direction))

let updateDirection (snake: Snake) =
    () // TODO: Implement

let updatePrize (snake: Snake) =
    () // TODO: Implement

let updateLocation (snake: Snake) =
    (); // TODO: Implement

let updateSnake (snake : Snake) =
    snake |> updateDirection |> updatePrize |> updateLocation
    

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
