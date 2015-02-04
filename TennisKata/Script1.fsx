#r @"..\packages\Unquote.2.2.2\lib\net40\Unquote.dll"
open Swensen.Unquote

type Player = PlayerA | PlayerB

type Points = Love | Fifteen | Thirty | Forty

type GameScore =
    | Points of Points * Points
    | Deuce
    | Advantage of Player
    | Game of Player

type PointsScored = Player list

let simpleScore point =
    match point with
    | Love -> Fifteen
    | Fifteen -> Thirty 
    | Thirty -> Forty
    | Forty -> invalidOp "Should never be trying to score from Forty"

let scoreDeuce points = if points = Points(Forty, Forty) then Deuce else points

let scorePoint currentScore player =
    match currentScore, player with
    | Points(Forty, _), PlayerA -> Game PlayerA
    | Points(_, Forty), PlayerB -> Game PlayerB

    | Points(pointsA, pointsB), PlayerA -> Points(simpleScore pointsA, pointsB) |> scoreDeuce
    | Points(pointsA, pointsB), PlayerB -> Points(pointsA, simpleScore pointsB) |> scoreDeuce
    
    | Deuce, player -> Advantage player

    | Advantage player, scorer when player = scorer -> Game player
    | Advantage _, _ -> Deuce

    | Game winner, _ -> Game winner


let rec scoreGame' currentScore pointsScored =
    match pointsScored with
    | [] -> currentScore
    | point :: points -> 
        let newScore = scorePoint currentScore point
        scoreGame' newScore points

let scoreGame = List.fold scorePoint (Points(Love, Love))

let sampleGame = [PlayerA; PlayerA; PlayerB]

sampleGame
    |> scoreGame

List.scan scorePoint (Points(Love, Love)) [PlayerA; PlayerA; PlayerB]

let createGame (input: string) =
    input.ToCharArray()
    |> Array.map (function | 'A' -> PlayerA | 'B' -> PlayerB | _ -> failwith "Bad point!")
    |> List.ofArray

test <@scorePoint (Points(Love, Love)) PlayerA = Points (Fifteen,Love)@>

let testScore input expected =
    let score =
        input
        |> createGame
        |> scoreGame
    test <@ score = expected @>


testScore "AAA" (Points(Forty, Love))
testScore "AAAA" (Game PlayerA)
testScore "AAABBB" Deuce
testScore "AAABBBB" (Advantage PlayerB)
testScore "AAABBBBA" (Deuce)
testScore "AAABBBBB" (Game PlayerB)

scorePoint Deuce PlayerA
scorePoint (Advantage PlayerB) PlayerA