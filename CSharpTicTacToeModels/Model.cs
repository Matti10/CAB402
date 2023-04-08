using System.Collections.Generic;
using FSharp;


namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => Player.Cross;
        public Player Nought => Player.Naught;
        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }

        //apply a given move to the gameState
        public Game ApplyMove(Game game, Move move)
        {
            game.gameGrid[move.Row][move.Col] = game.Turn;
            game.Turn = getPlayer(game);

            return game;

        }

        //create move object
        public Move CreateMove(int row, int col)
        {
            return new Move(row, col);
        }

        //return a score for a given game condition
        public int Heuristic(Game game, Player perspective)
        {
            (Player player, string result, List<System.Tuple<int, int>> moves) = GameOutcomeDecoded(game);
            if (result == "W")
            {
                if (player == perspective)
                {
                    return 1;
                }
                else
                {
                    return -1;
                }
            }
            else
            {
                return 0;  
            }
        }

        //get player whose turn it is
        public Player getPlayer(Game game)
        {
            Player nextTurn;
            if (game.Turn == Cross)
                nextTurn = Nought;
            else
                nextTurn = Cross;
            
            return nextTurn;
        }

        //check if the game is over
        public bool gameOver(Game game)
        {
            (Player player, string result, List<System.Tuple<int, int>> moves) = GameOutcomeDecoded(game);
            if (result == "W" || result == "D")
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        //generate all applicable moves 
        public List<Move> moveGen(Game game)
        {
            List<Move> moves = new List<Move>();

            for (int i = 0; i < game.Size; i++)
            {
                for (int j = 0; j < game.Size; j++)
                {
                    if (game.getPiece(i, j) == "")
                        moves.Add(new Move(i, j));
                }
            }

            return moves;
        }

        //find the best move from avaliable moves
        public Move FindBestMove(Game game)
        {
            //MiniMax to find the best move
            (Move,int) MiniMax(Game game, Player perspective)
            {
                NodeCounter.Increment();
                //if game is over, return the heuristic of the game state
                if(gameOver(game))
                {
                    return (new Move(-1, -1), Heuristic(game, perspective));
                }

                if (perspective == Cross) //if maximiser
                {
                    List<Move> moves = moveGen(game); //generate moves

                    //intialise output vars
                    int bestScore = -2;
                    Move bestMove = new Move(-1, -1);
                    //call minimax on all moves
                    foreach (Move move in moves)
                    {
                        (Move somemove,int score) = MiniMax(ApplyMove(game, move), getPlayer(game));
                        if (score > bestScore) //maximiser
                        {
                            bestScore = score;
                            bestMove = move;
                        }
                    }

                    return (bestMove, bestScore);
                }
                else
                {
                    List<Move> moves = moveGen(game); //generate moves

                    //intialise output vars
                    int bestScore = -2;
                    Move bestMove = new Move(-1, -1);
                    //call minimax on all moves
                    foreach (Move move in moves)
                    {
                        (Move somemove, int score) = MiniMax(ApplyMove(game, move), getPlayer(game));
                        if (score > bestScore) //minimiser
                        {
                            bestScore = score;
                            bestMove = move;
                        }
                    }

                    return (bestMove, bestScore);
                }
            }
            
            NodeCounter.Reset();
            
            (Move bestMove, int someScore) = MiniMax(game, game.Turn);

            return bestMove;
                           
        }

        //get all lines in game
        (Player, (int, int))[][] getLines(Game game)
        {
            (Player, (int, int))[][] lines = new (Player, (int, int))[(game.Size * 2) + 2][];

            for (int i = 0; i < lines.Length; i++)
            {
                lines[i] = new (Player, (int, int))[game.Size];
            }


            for (int i = 0; i < game.Size; i++)
            {
                for (int j = 0; j < game.Size; j++)
                {
                    //get rows
                    lines[i][j] = (game.gameGrid[j][i], (i, j));
                    //get cols
                    lines[i + game.Size][j] = (game.gameGrid[i][j], (i, j));

                }
                //get diags
                lines[game.Size * 2][i] = (game.gameGrid[i][i], (i, i));
                lines[game.Size * 2 + 1][i] = (game.gameGrid[i][game.Size - i - 1], (i, game.Size - i));
            }

            return lines;
        }

        //output winner in data format thats more usuable than TicTacToeOutcome<Player>
        public (Player, string, List<System.Tuple<int, int>>) GameOutcomeDecoded(Game game)
        {
            
            //find result of a finished game
            (Player, string, List<System.Tuple<int, int>>) getResults((Player, (int, int))[][] lines, Game game)
            {
                foreach ((Player, (int, int))[] line in lines)
                {
                    //count number of 'X', 'O' and empty cells in each line
                    int XCount = 0;
                    int OCount = 0;
                    int ECount = 0;

                    List<System.Tuple<int, int>> moveList = new List<System.Tuple<int, int>>(game.Size);


                    foreach ((Player, (int, int)) move in line)
                    {
                        (Player player, (int, int) location) obj = move;
                        if (obj.player == Cross)
                        {
                            XCount++;
                        }
                        else if (obj.player == Nought)
                        {
                            OCount++;
                        }
                        else
                        {
                            ECount++;
                        }
                        (int x, int y) = obj.location;

                        moveList.Add(System.Tuple.Create(x, y));


                    }

                    //decide outcome
                    if (XCount == game.Size)//if x count is the game size, its a win
                    {
                        return (Cross, "W", moveList);
                    }
                    else if (OCount == game.Size)
                    {
                        return (Nought, "W", moveList);//if o count is the game size, its a win
                        
                    }
                    else if (ECount == 0)
                    {
                        return (Nought, "D",moveList); //if there are no empty cells its a draw
                    }
                }

                return (Nought,"U",new List<System.Tuple<int, int>>()); // if none of the the above conditions are met, game is still undecided
            }

            return getResults(getLines(game), game);

        }

        //code a "decoded" outcome to something useable by the interface
        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            //cast "decoded" to TicTacToeOutcome<Player
            TicTacToeOutcome<Player> codeOutcome ((Player, string, List<System.Tuple<int, int>>) deCodedOutcome)
            {
                (Player player, string outcome, List<System.Tuple<int, int>> moves) = deCodedOutcome;

                if (outcome == "W")
                {
                    return TicTacToeOutcome<Player>.Win.NewWin(player, moves);
                }
                else if (outcome == "D")
                {
                    return TicTacToeOutcome<Player>.Draw;
                }
                else
                    return TicTacToeOutcome<Player>.Undecided;
            }

            return codeOutcome(GameOutcomeDecoded(game));
        }

        //intiate game
        public Game GameStart(Player first, int size)
        {
            return new Game(size, first);
        }
    }
}