#r @"..\packages\Unquote.2.2.2\lib\net40\Unquote.dll"
open Swensen.Unquote

type Player = PlayerA | PlayerB

type Points = Love | Fifteen | Thirty | Forty

type GameScore =
    | Points of Points * Points
    | Deuce
    | Advantage of Player
    | Game of Player

let scorePoint currentScore player =
    match currentScore, player with
    | Points(Love, pointsB), PlayerA -> Points(Fifteen, pointsB)
    | Points(Fifteen, pointsB), PlayerA -> Points(Thirty, pointsB)
    | Points(Thirty, Forty), PlayerA -> Deuce
    | Points(Thirty, pointsB), PlayerA -> Points(Forty, pointsB)
    | Points(Forty, _), PlayerA -> Game PlayerA
    
    | Advantage player, scorer when player = scorer -> Game player

    | Deuce, player -> Advantage player


    | Advantage PlayerA, PlayerB -> Deuce
    | Advantage PlayerB, PlayerA -> Deuce

    | Points(pointsA, Love), PlayerB -> Points(pointsA, Fifteen)
    | Points(pointsA, Fifteen), PlayerB -> Points(pointsA, Thirty)
    | Points(Forty, Thirty), PlayerB -> Deuce
    | Points(pointsA, Thirty), PlayerB -> Points(pointsA, Forty)
    | Points(_, Forty), PlayerB -> Game PlayerB
    

    | Game winner, _ -> Game winner

test <@scorePoint (Points(Love, Love)) PlayerA = Points (Fifteen,Love)@>
scorePoint Deuce PlayerA
scorePoint (Advantage PlayerB) PlayerA