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

let scorePoint currentScore player =
    match currentScore, player with
    | Points(Love, pointsB), PlayerA -> Points(Fifteen, pointsB)
    | Points(Fifteen, pointsB), PlayerA -> Points(Thirty, pointsB)
    | Points(Thirty, Forty), PlayerA -> Deuce
    | Points(Thirty, pointsB), PlayerA -> Points(Forty, pointsB)
    | Points(Forty, _), PlayerA -> Game PlayerA
    
    | Advantage player, scorer when player = scorer -> Game player
    | Advantage _, _ -> Deuce

    | Deuce, player -> Advantage player

    | Points(pointsA, Love), PlayerB -> Points(pointsA, Fifteen)
    | Points(pointsA, Fifteen), PlayerB -> Points(pointsA, Thirty)
    | Points(Forty, Thirty), PlayerB -> Deuce
    | Points(pointsA, Thirty), PlayerB -> Points(pointsA, Forty)
    | Points(_, Forty), PlayerB -> Game PlayerB

    | Game winner, _ -> Game winner


let rec scoreGame currentScore pointsScored =
    match pointsScored with
    | [] -> currentScore
    | point :: xs -> 
        let newScore = scorePoint currentScore point
        scoreGame newScore xs

let runGame = List.fold scorePoint (Points(Love, Love))

let sampleGame = [PlayerA; PlayerA; PlayerB]

sampleGame
    |> scoreGame (Points(Love, Love))

List.scan scorePoint (Points(Love, Love)) [PlayerA; PlayerA; PlayerB]

let createGame (input: string) =
    input.ToCharArray()
    |> Array.map (function | 'A' -> PlayerA | 'B' -> PlayerB | _ -> failwith "Bad point!")
    |> List.ofArray

let testScore input expected =
    let score =
        input
        |> createGame
        |> runGame
    test <@ score = expected @>

testScore "AAAA" (Game PlayerA)

test <@scorePoint (Points(Love, Love)) PlayerA = Points (Fifteen,Love)@>

scorePoint Deuce PlayerA
scorePoint (Advantage PlayerB) PlayerA