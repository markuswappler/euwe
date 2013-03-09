module SwissDraw

type Score = 
    | NotPlayed
    | Score of int * int

type Game(home, away, score) =
    member self.home = home
    member self.away = away
    member self.score = score
    member self.scoreOf team =
        match score with
        | NotPlayed -> NotPlayed
        | Score(h, a) -> if team = home then Score(h, a)
                         else if team = away then Score(a, h)
                         else NotPlayed   

[<AbstractClass>]
type Evaluator<'Eval> () =    
    abstract rule : string -> Game -> 'Eval
    abstract aggregate : 'Eval -> 'Eval -> 'Eval
    abstract init : string -> 'Eval    
    member self.apply (games : Game list) team =
        games |> List.fold (fun eval game -> game |> self.rule team |> self.aggregate eval) 
                           (self.init team)

[<AbstractClass>]
type Ranking<'Eval>(games, teams, evaluator : Evaluator<'Eval>) as self =    
    let evaluation = lazy (teams |> List.map (fun team -> (team, evaluator.apply games team))
                                 |> List.sortWith (fun (_, eval1) (_, eval2) -> self.compare eval1 eval2)
                                 |> List.mapi (fun i (team, eval) -> (i + 1, team, eval)))
    abstract compare : 'Eval -> 'Eval -> int
    member self.all = evaluation.Value
    member self.evalOf team = evaluation.Value 
                              |> List.find (fun (_, t, _) -> t = team)
                              |> fun (_, _, eval) -> eval

[<AbstractClass>]
type Scheduler(teams) as self =
    let rec findOpponents team teams =
        match teams with
        | [] -> []
        | opp :: rest -> let opps = findOpponents team rest
                         if self.isValid (team, opp) then opp :: opps else opps

    let rec findMatching teams =
        match teams with
        | [] -> Some([])
        | team :: rest -> match tryOpponents team (findOpponents team rest) rest with
                          | None -> if List.length rest % 2 = 0 then findMatching rest 
                                    else None
                          | Some(matching) -> Some(matching)

    and tryOpponents team opponents rest =
        match opponents with
        | [] -> if List.length rest % 2 = 0 then findMatching rest
                else None
        | opp :: opps -> match findMatching (rest |> List.filter (fun t -> t <> opp)) with
                         | None -> tryOpponents team opps rest
                         | Some(matching) -> Some((team, opp) :: matching) 

    let matching = lazy (match findMatching teams with
                         | None -> []
                         | Some(matching) -> matching |> List.map (fun (team1, team2) -> Game(team1, team2, NotPlayed)))
    
    member self.nextGames = matching.Value
    abstract isValid : string * string -> bool

type VictoryPointsEvaluator() =
    inherit Evaluator<int>()
    override self.rule team game = 
        match game.scoreOf team with
        | NotPlayed -> 0
        | Score(team, opp) -> let diff = team - opp
                              8 + if diff > 7 then 7
                                  else if diff < -7 then -7
                                  else diff
    override self.aggregate eval1 eval2 = eval1 + eval2
    override self.init team = 0

type VictoryPointsRanking(teams, games) =
    inherit Ranking<int>(games, teams, VictoryPointsEvaluator())
    override self.compare eval1 eval2 = eval2 - eval1

type BuchholzEvaluator(ranking : VictoryPointsRanking) =
    inherit Evaluator<int * int>()
    override self.rule team game =
        if team = game.home then (0, ranking.evalOf game.away)
        else if team = game.away then (0, ranking.evalOf game.home)
        else (0, 0)
    override self.aggregate eval1 eval2 = (fst eval1 + fst eval2, snd eval1 + snd eval2)
    override self.init team = (ranking.evalOf team, 0)
    
type BuchholzRanking(teams, games) =
    inherit Ranking<int * int>(games,  teams, BuchholzEvaluator(VictoryPointsRanking(teams, games)))
    override self.compare eval1 eval2 =
        if fst eval1 = fst eval2 then snd eval2 - snd eval1
        else fst eval2 - fst eval1        

type SwissDraw(teams, games) =
    inherit Scheduler(teams)
    let isAnotherGame (team1, team2) (game : Game) =
        ((game.home <> team1) && (game.home <> team2)) 
        || ((game.away <> team1) && (game.away <> team2))
    override self.isValid (team1, team2) = 
        games |> List.forall (isAnotherGame (team1, team2))