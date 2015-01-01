open System

let random = new Random();

// Structure
type Snake = {
    direction : int * int
    size: int * int
    prize: int * int
    location: (int * int) list
    isDead: bool
}

type Move = 
    | None
    | Up
    | Down
    | Left
    | Right

// Create new snake
let getRandomPosition (width, height) =
    (random.Next width, random.Next height)

let initSnake size = {
    direction = (0, 1)
    size = size
    prize = getRandomPosition size
    location = [(fst size / 2, 0)]
    isDead = false
};

// Update snake
let getNewHead (snake: Snake) =
    let oldHead = snake.location.Head
    (fst oldHead + fst snake.direction, snd oldHead + snd snake.direction)

let (|Horizontal|Vertical|) (direction: int * int) =
    if snd direction = 0
    then Horizontal
    elif fst direction = 0
    then Vertical
    else failwith "Incorrect snake position"

let updateDirection (move: Move) (snake: Snake) =
    let newDirection = 
        match (move, snake.direction) with
        | (Up, Horizontal) -> (0, 1)
        | (Down, Horizontal) -> (0, -1)
        | (Left, Vertical) -> (-1, 0)
        | (Right, Vertical) -> (-1, 0)
        | _ -> snake.direction
    { snake with direction = newDirection }

let updatePrize (snake: Snake) =
    if getNewHead snake = snake.prize
    then { snake with prize = getRandomPosition snake.size }
    else snake

let updateLocation (snake: Snake) =
    () // TODO: Implement

let updateIsDead = 
    () // TODO: Implement

let updateSnake (snake : Snake) (move: Move) =
    snake |> updateDirection move |> updatePrize |> updateLocation |> updateIsDead
    

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
