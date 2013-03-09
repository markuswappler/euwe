module Io
open System
open System.IO
open SwissDraw

let parseGame (text : string) = 
    let tokens = text.Split [|'-'; ','; ':'|] |> Array.map (fun s -> s.Trim())
    Game(tokens.[1], tokens.[2], Score(Int32.Parse tokens.[3], Int32.Parse tokens.[4]))   

let serializeGame number (game : Game) =
    let score =
        match game.score with
        | NotPlayed -> "? : ?"
        | Score(h, a) -> h.ToString() + " : " + a.ToString()
    let number = (number + 1).ToString().PadLeft 3
    let game = (game.home + " - " + game.away).PadRight 35
    "Game" + number + " , " + game + " , " + score

let serializeRanking (rank, team : string, (victoryPoints, buchholz)) =
    let rank = rank.ToString().PadLeft 3
    let team = team.PadRight 16
    let victoryPoints = victoryPoints.ToString().PadLeft 2
    let buchholz = buchholz.ToString().PadLeft 2
    "Rank" + rank + " , " + team + " , " + victoryPoints + " , " + buchholz

let parseRound (path : string) = path.Split '\\' 
                                 |> Array.rev
                                 |> fun path -> Array.get path 0
                                 |> fun file -> file.Substring (5, 1)
                                 |> Int32.Parse        

let loadTeams file = File.ReadAllLines file
                     |> List.ofArray 
                     |> List.map (fun s -> s.Trim())

let saveRound file ranking games = 
    let rankingLines = ranking |> List.map serializeRanking
    let gameLines = games |> List.mapi serializeGame
    File.WriteAllLines(file, rankingLines @ [""] @ gameLines)

let getRound pattern = 
    let dir = Directory.GetCurrentDirectory()
    let files = Directory.GetFiles(dir, pattern)
    if files |> Array.length = 0 then None
    else files 
         |> Array.map parseRound 
         |> Array.max 
         |> Option.Some

let loadGames file = File.ReadAllLines file             
                     |> List.ofArray
                     |> List.filter (fun line -> line.StartsWith("Game"))
                     |> List.map parseGame