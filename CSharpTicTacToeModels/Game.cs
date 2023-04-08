using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public int Size; //size of game
        public Player Turn; //current turn
        public Player[][] gameGrid; //grid of game cells

        Player ITicTacToeGame<Player>.Turn => Turn;
        int ITicTacToeGame<Player>.Size => Size;

        //gets the string representation of a piece in the game array
        public string getPiece(int row, int col) 
        {
            if (gameGrid[row][col] == Player.Naught)
                return "O";
            else if (gameGrid[row][col] == Player.Cross)
                return "X";
            else
                return "";
        }
        
        //game constructor
        public Game(int size,Player firstPlayer)
        {
            Size = size;
            Turn = firstPlayer;
            gameGrid = new Player[size][];
            for (int i = 0; i < size; i++)
            {
                gameGrid[i] = new Player[size];
            }
        }

    }
}