open SwissDraw
open Io

let teams = loadTeams "Teams.swiss"
let (nextRound, games) =
    match getRound "Round*.swiss" with 
    | None -> (1, [])
    | Some(round) -> (round + 1, loadGames ("Round" + round.ToString() + ".swiss"))

let ranking = BuchholzRanking(teams, games)
let orderedTeams = ranking.all |> List.map (fun (_, team, _) -> team)
let newGames = games @ SwissDraw(orderedTeams, games).nextGames

saveRound ("Round" + nextRound.ToString() + ".swiss") ranking.all newGames