namespace QUT

    module GameTheory =
        
        

        //type GameObj = { game:int ; move : char }
        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =

            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMaxDriver (game :'Game) (perspective : 'Player) (isMaximiser) =
                NodeCounter.Increment()

                if (gameOver game) then
                    let aThing = gameOver game
                    ((None : Option<'Move>), heuristic game perspective)
                else
                    if isMaximiser then
                        //get all moves for current state
                        let moves = moveGenerator game

                        //Execute MiniMax on the resulting game of each move
                        Seq.map (fun (move : 'Move) -> MiniMaxDriver (applyMove game move) (getTurn(applyMove game move)) false) moves
                        //map move onto heuristic result
                        |> Seq.map2 (fun move (_,res) -> (Some move,res)) moves
                        //Remove all elements other than the best result
                        |> Seq.reduce (fun (m,x) (n,y) -> if x >= y then (m,x) else (n,y))

                    else
                        //get all moves for current state
                        let moves = moveGenerator game

                        //Execute MiniMax on the resulting game of each move
                        Seq.map (fun (move : 'Move) -> MiniMaxDriver (applyMove game move) (getTurn(applyMove game move)) true) moves
                        //map move onto heuristic result
                        |> Seq.map2 (fun move (_,res) -> (Some move,res)) moves
                        //Remove all elements other than the best result
                        |> Seq.reduce (fun (m,x) (n,y) -> if x <= y then (m,x) else (n,y))
   

            let MiniMax (game)(perspective) =
                let isMaximiser = if perspective = getTurn game then true else false
                
                MiniMaxDriver game perspective isMaximiser

            NodeCounter.Reset()
            
            MiniMax
            
            
               

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMaxDriver alpha beta (game :'Game) (perspective : 'Player) (isMaximiser : bool) =
                NodeCounter.Increment()

                if (gameOver game) then
                    ((None : Option<'Move>), heuristic game perspective)
                else
                    if isMaximiser then
                        //get all moves for current state
                        let moves = moveGenerator game

                        //Execute MiniMax on the resulting game of each move
                        Seq.map (fun (move : 'Move) -> MiniMaxDriver alpha beta (applyMove game move) (getTurn(applyMove game move)) false) moves
                        //map move onto heuristic result
                        |> Seq.map2 (fun move (_,res) -> (Some move,res)) moves
                        //Remove all elements other than the best result
                        |> Seq.reduce (fun (m,x) (n,y) -> if x >= y then (m,x) else (n,y))

                    else
                        //get all moves for current state
                        let moves = moveGenerator game

                        //Execute MiniMax on the resulting game of each move
                        Seq.map (fun (move : 'Move) -> MiniMaxDriver alpha beta (applyMove game move) (getTurn(applyMove game move)) true) moves
                        //map move onto heuristic result
                        |> Seq.map2 (fun move (_,res) -> (Some move,res)) moves
                        //Remove all elements other than the best result
                        |> Seq.reduce (fun (m,x) (n,y) -> if x <= y then (m,x) else (n,y))
   

            let MiniMax (alpha)(beta)(game)(perspective)  =
                let isMaximiser = if perspective = getTurn game then true else false
                
                MiniMaxDriver alpha beta game perspective isMaximiser


            NodeCounter.Reset()
            MiniMax
