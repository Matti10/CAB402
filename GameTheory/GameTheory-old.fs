namespace QUT

    module GameTheory =
        
        //type GameObj = { game:int ; move : char }
        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax  (game) (perspective) (move : Option<'Move>)  =
                NodeCounter.Increment()
                if gameOver game then
                    (move , heuristic)
                  
                
                //maximising player (fred)
                else if NodeCounter.Count % 2 = 0 then //this is wrong
                    let bestValue = (moveGenerator,-1000)

                    for option in moveGenerator game do
                        let (move, score) = MiniMax (applyMove game option) (getTurn game) (move) 
                        bestValue = (move, (max (score game (getTurn game)) bestValue))
                        

                        bestValue
                        //need to return something in for expr
                            
                    ou

                        
                //minimising player (jill)
                else
                    let bestValue = 1000

                    for move in moveGenerator game do
                        let value = MiniMax (applyMove game move) (getTurn game)
                        bestValue = min value bestValue

                    bestValue
            
            

            NodeCounter.Reset()

            //let gameObj: GameObj = { game = 0; move = 'Z' }

            //MiniMax gameObj (getTurn 0)
            MiniMax 0 (getTurn 0)


        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()
                raise (System.NotImplementedException("Alpha Beta Pruning"))
            NodeCounter.Reset()
            MiniMax
