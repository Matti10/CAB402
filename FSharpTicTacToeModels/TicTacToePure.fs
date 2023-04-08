namespace QUT

    module FSharpPureTicTacToeModel =
        
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            { 
                row: int;
                col: int;
            }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { 
                turn : Player;
                size : int;
                piece: Move;
                previousGame : Option<GameState>;
                pieceList : List<Move*Player>
            }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size
                member this.getPiece(row, col) = 
                    //get peice in that position in the game array
                    let helper (move,player) =
                        if (move.row,move.col) = (row, col) 
                        then
                            if player = Cross 
                            then "O"
                            else "X"
                        else "E"

                    let helperList = List.map helper this.pieceList |> List.filter (fun x -> x <> "E") 
                    
                    if helperList |> List.length > 0 then helperList |> List.head else ""



        //new Move object
        let CreateMove row col = 
            {row=row;col=col}
            
        //apply a move by creating a new gamestate, with links to the past one
        let ApplyMove (oldState:GameState) (move: Move) = 
             {
                turn = (if oldState.turn = Cross then Nought else Cross) //change turn
                size = oldState.size
                piece = move //new move
                previousGame = Some(oldState) //link to old game
                pieceList = [(move,(if oldState.turn = Cross then Nought else Cross))] @ oldState.pieceList //list of all pieces
             }

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [seq[(0,0);(0,1)];seq[(1,0);(1,1)];seq[(0,0);(1,0)];seq[(0,1);(1,1)];seq[(0,0);(1,1)];seq[(0,1);(1,0)]]
        // The order of the lines and the order of the squares within each line does not matter
        let Lines (size:int) (game : GameState) : seq<seq<int*int>> =
            let sizeSeq = seq {0..size-1}

            seq {
                yield! Seq.map (fun num -> (Seq.map (fun x -> (num,x))) sizeSeq)  sizeSeq;//horizontals
                yield! Seq.map (fun num -> (Seq.map (fun x -> (x,num))) sizeSeq)  sizeSeq;//verticals
                Seq.map (fun x -> (x,x)) sizeSeq;//diag 1
                Seq.map (fun x -> (x,size-x-1)) sizeSeq //diag 2         
            }
        
        //generate all coordinates of a board
        let getEmptyBoard size =
            let sizeSeq = seq {0..size-1}

            Seq.map (fun num -> (Seq.map (fun x -> (num,x))) sizeSeq)  sizeSeq;

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = (*raise (System.NotImplementedException("CheckLine"))*)
            
            //recursively search for a piece
            let rec getPiece (game : GameState) (row,col) =
                if (game.piece.row,game.piece.col) = (row,col) then
                    if game.turn = Cross then "X" else "O"
                    
                else
                    if game.previousGame <> None
                    then getPiece game.previousGame.Value (row,col)
                    else "E"
                        
            //count the amout of moves in each line
            let XCount = (Seq.map (getPiece game) line) |> Seq.filter (fun elem -> elem = "X") |> Seq.length
            let OCount = (Seq.map (getPiece game) line) |> Seq.filter (fun elem -> elem = "O") |> Seq.length
            let ECount = (Seq.map (getPiece game) line) |> Seq.filter (fun elem -> elem = "E") |> Seq.length
            

            if XCount = game.size //X win
            then Win(Cross, line)
            else if OCount = game.size  //O win
            then Win(Nought, line)
            else if ECount = 0 //Draw
            then Draw
            else Undecided //Undecided

        //find outcome of given game state
        let GameOutcome game =
            //call check line on all lines in game
            let resultMap = Seq.map (fun line -> CheckLine game line) (Lines game.size game)
            
            //count number of wins
            let winMap = Seq.filter (fun (x : TicTacToeOutcome<Player>) -> x <> Undecided && x <> Draw) resultMap
            //count number of undecided
            let UMap = Seq.filter (fun (x : TicTacToeOutcome<Player>) -> x = Undecided) resultMap
            

            if winMap |> Seq.length > 0 //if there is a win
            then winMap |> Seq.head //return the win
            else 
                if UMap |> Seq.length > 0
                then Undecided // if theres any undecied, game is undecided still
                else Draw //otherwise its a draw
        
        //getPiece using gameArray rather than recursively
        let rec getPiece (game : GameState) (col,row) =
            if (game.piece.row,game.piece.col) = (row,col) then
                if game.turn = Cross then "X" else "O"
            else 
                getPiece game.previousGame.Value (row,col)

        //init the game
        let GameStart (firstPlayer:Player) size = 
            {
                turn = firstPlayer
                size = size
                piece = CreateMove -1 -1
                previousGame = None
                pieceList = [(CreateMove -1 -1),firstPlayer] //set previous move to be in -1,-1 (this is always filtered)
                
            }

        

        //get score for given game state
        let Heuristic game perspective =
            match GameOutcome game with
            | Win(player, _) -> 
                if player = perspective then 1 else -1
            | _ -> 0
        
        //gets the next player
        let getPlayer (game : GameState) : Player =
            let (_,prevPlayer) = game.pieceList |> List.head
            if prevPlayer = Cross then Nought else Cross 
   
        //check if game is over
        let gameOver (game: GameState) : bool =
            match GameOutcome game with
            | Undecided -> false
            | _ -> true

        //generate all applicable moves
        let moveGen (game: GameState) =
            let currentMoves = Seq.map (fun (x,_) -> (x.row,x.col)) game.pieceList

            //generate coords of board
            getEmptyBoard game.size
            |> Seq.concat 
            |> Seq.map (fun (x,y) -> if currentMoves |> Seq.contains (x,y) then (-1,-1) else (x,y)) //if cell isnt taked return it
            |> Seq.filter (fun (x,y) -> (x,y) <> (-1,-1)) //filter out invalid cells
            |> Seq.map (fun (x,y) -> CreateMove x y) // cast to moves
        

        //call minimax
        let MiniMax game = GameTheory.MiniMaxGenerator Heuristic getPlayer gameOver moveGen ApplyMove (game)
        
        //call minimax with pruning
        let MiniMaxAlphaBeta (game:GameState) = GameTheory.MiniMaxWithAlphaBetaPruningGenerator (Heuristic) (getPlayer) (gameOver) (moveGen) (ApplyMove) 0 0 (game) 
        
        

        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) = match MiniMax game game.turn with
                                               | (Some(x),_) -> x
                                               | (None,_) -> raise (System.ArgumentNullException("Things have gone south..."))


                

        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = match MiniMaxAlphaBeta game game.turn with
                                               | (Some(x),_) -> x
                                               | (None,_) -> raise (System.ArgumentNullException("Things have gone south..."))