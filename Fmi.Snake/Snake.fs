﻿open System

let random = new Random();

// Structure
type Snake = {
    direction : int * int
    size: int * int
    prize: int * int
    location: (int * int) list
    isAlive: bool
    hasPrize: bool
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
    direction = 0, -1
    size = size
    prize = getRandomPosition size
    location = [(fst size / 2, (snd size) - 1)]
    isAlive = true
    hasPrize = false
};

// Update snake
let getNewHead (snake: Snake) =
    let (hx, hy) = snake.location.Head
    let (dx, dy) = snake.direction
    (hx + dx, hy + dy)

let (|Horizontal|Vertical|) (direction: int * int) =
    match direction with
    | (_, 0) -> Horizontal
    | (0, _) -> Vertical
    | _ -> failwith "Incorrect direction"

let updateDirection (move: Move) (snake: Snake) =
    let newDirection = 
        match (move, snake.direction) with
        | (Up, Horizontal) -> 0, -1
        | (Down, Horizontal) -> 0, 1
        | (Left, Vertical) -> -1, 0
        | (Right, Vertical) -> 1, 0
        | _ -> snake.direction
    { snake with direction = newDirection }

let rec withoutLastElement list =
    match list with
    | a :: b when b.Length > 0 -> a :: withoutLastElement b
    | _ -> []

let updateLocation (snake: Snake) =
    let l = snake.location
    let newHead = getNewHead snake
    if snake.hasPrize
    then { snake with location = newHead :: withoutLastElement l ; hasPrize = false }
    else { snake with location = newHead :: l }

let updatePrize (snake: Snake) =
    if snake.location.Head = snake.prize
    then { snake with prize = getRandomPosition snake.size; hasPrize = true }
    else snake

let updateIsDead (snake: Snake) = 
    match snake.location.Head with
    | (x, _) when x < 0 || x >= fst snake.size -> { snake with isAlive = false }
    | (_, y) when y < 0 || y >= snd snake.size -> { snake with isAlive = false }
    | _ -> snake


let updateSnake (snake : Snake) (move: Move) =
    snake 
    |> updateDirection move 
    |> updateLocation 
    |> updatePrize 
    |> updateIsDead
    
// UI
let printSnake (snake: Snake) =
    let (width, height) = snake.size
    for i = 0 to width do
        let line = [
            for j in 0 .. height - 1 ->
                let c = (i, j)
                if c = snake.prize then "*"
                elif List.exists (fun e -> e = c) snake.location then "@"
                else " "
        ]

        line 
        |> List.toArray 
        |> (fun s -> String.Join("", s) ) 
        |> Console.WriteLine


let getMove (key: ConsoleKeyInfo) = 
    match key.KeyChar with
    | 'w' -> Up
    | 's' -> Down
    | 'a' -> Left
    | 'r' -> Right
    | _ -> None

[<EntryPoint>]
let main argv = 
    let size = 10, 10
    let mutable snake = initSnake size
    while snake.isAlive do
        snake 
        |> printSnake 
        |> ignore

        snake <- Console.ReadKey false 
                 |> getMove 
                 |> updateSnake snake

    0