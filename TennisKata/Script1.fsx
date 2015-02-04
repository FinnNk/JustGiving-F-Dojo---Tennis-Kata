
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
    | Deuce, PlayerA -> Advantage PlayerA
    | Advantage PlayerA, PlayerA -> Game PlayerA