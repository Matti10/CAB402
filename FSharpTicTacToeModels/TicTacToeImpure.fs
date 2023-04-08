namespace QUT

    module FSharpImpureTicTacToeModel =
    
        type Player = None | Nought | Cross

        //move type
         type Move = 
            { 
               row : int
               col : int
            }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

      //define a gamestte
        type  GameState =    
            {
                mutable turn : Player
                size : int
                mutable gameArray : array<array<Player>> //board represented as 2D array
            }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size
                //return peice in given coord on gameboard
                member this.getPiece(row, col) = if this.gameArray[row][col] = Cross 
                                                 then "X" 
                                                 else if this.gameArray[row][col] = Nought 
                                                 then "O" 
                                                 else ""
        
        //get all lines in the board
        let getLines game =
            let sizeSeq = seq {0..game.size-1}

            seq {
                yield! Seq.map (fun num -> (Seq.map (fun x -> (num,x))) sizeSeq)  sizeSeq;
                yield! Seq.map (fun num -> (Seq.map (fun x -> (x,num))) sizeSeq)  sizeSeq;
                Seq.map (fun x -> (x,x)) sizeSeq;
                Seq.map (fun x -> (x,(game.size-x)-1)) sizeSeq           
            }
        
        //get coord of all cells in board
        let getEmptyBoard size =
            let sizeSeq = seq {0..size-1}

            Seq.map (fun num -> (Seq.map (fun x -> (num,x))) sizeSeq)  sizeSeq;
        
        //check if a line is a win
        let checkLine line (game:GameState) =
                //count "X" and "O"
                let XCount = (line |> Array.filter (fun (x,y) -> game.gameArray[x][y] = Cross) |> Array.length)
                let OCount  = (line |> Array.filter (fun (x,y) -> game.gameArray[x][y] = Nought) |> Array.length)
                
                //establish the result
                if XCount = game.size
                then Win(Cross, line)
                else if OCount = game.size
                then Win(Nought, line)
                else if (XCount+OCount) = game.size
                then Draw
                else Undecided

        //find game outcome
        let GameOutcome (game: GameState) = (*raise (System.NotImplementedException("ApplyMove"))*)
            //check all lines
            let resultMap = Seq.map (fun line -> checkLine (Seq.toArray line) game) (getLines game)
            
            //count wins and undecided
            let winMap = Seq.filter (fun (x : TicTacToeOutcome<Player>) -> x <> Undecided && x <> Draw) resultMap
            let UMap = Seq.filter (fun (x : TicTacToeOutcome<Player>) -> x = Undecided) resultMap
            
            //establish result
            if winMap |> Seq.length > 0
            then winMap |> Seq.head
            else 
                if UMap |> Seq.length > 0
                then Undecided
                else Draw
         
        //gets next turn
        let getTurn game =
            if game.turn = Cross then Nought else Cross
        
        //apply a move to the game
        let ApplyMove game move  = 
            //writes move to game array and moves to next player
            Array.set game.gameArray[move.row] move.col game.turn
            game.turn <- getTurn game

            game

        //Undo a move
        let UndoMove game move =
            //set player to the other player
            game.turn <- getTurn game
            //set cell in gamearray back to None
            game.gameArray[move.row][move.col] <- None

            move

        //create a move object
        let CreateMove row col = 
            {row=row;col=col}

        //init the gameState
        let GameStart first size = 
            {
                turn = first
                size = size
                gameArray = [|for i in 0..(size-1) -> [|for j in 0..(size-1) -> None|]|]
            }

        //check is the game is over
        let gameOver game =
            if GameOutcome game = Undecided then false else true

        //get score of given gamestate
        let heuristic game perspective =
            match GameOutcome game with
            | Win(player, _) -> if player = perspective then 1 else -1
            | _ -> 0

        //get applicable moves
        let getMoves game = 
                        
            //return any cells that aren't empty
            getEmptyBoard game.size
            |> Seq.concat 
            |> Seq.map (fun (x,y) -> if game.gameArray[x][y] = None then (x,y) else (-1,-1))
            |> Seq.filter (fun (x,y) -> (x,y) <> (-1,-1))
            |> Seq.map (fun (x,y) -> CreateMove x y)
            |> Seq.toArray

        //find best move from current game state
        let FindBestMove game =  
            //use minimax to find the best move
            let rec MiniMax game =
                NodeCounter.Increment()
                if gameOver game then
                    (CreateMove -1 -1, heuristic game game.turn)
                else
                    if game.turn = Cross then
                        //get all moves for current state
                        let moves = getMoves game
                        
                        //Execute MiniMax on the resulting game of each                                                
                        Array.map (fun move -> MiniMax (ApplyMove game move)) moves
                        //map move onto heuristic result
                        |> Array.map2 (fun x (_,y) -> (UndoMove game x,y)) moves
                        //Remove all elements other than the best result
                        |> Array.reduce (fun (m,x) (n,y) -> if x >= y then (m,x) else (n,y))
                    else
                        //get all moves for current state
                        let moves = getMoves game
                        
                        //Execute MiniMax on the resulting game of each                                                
                        Array.map (fun move -> MiniMax (ApplyMove game move)) moves
                        //map move onto heuristic result
                        |> Array.map2 (fun x (_,y) -> (UndoMove game x,y)) moves
                        //Remove all elements other than the best result
                        |> Array.reduce (fun (m,x) (n,y) -> if x <= y then (m,x) else (n,y))
            
            let (move,_) = MiniMax game //call Minimax

            NodeCounter.Reset()
            move //return the best move

                        


        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross
                member this.Nought with get()            = Nought
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game  move
                member this.FindBestMove(game)           = FindBestMove game
