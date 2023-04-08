namespace QUT.CSharpTicTacToe
{
    public class Move : ITicTacToeMove
    {
        public int Row ;
        public int Col;
        int ITicTacToeMove.Row => Row;

        int ITicTacToeMove.Col => Col;

        //constructor
        public Move(int row, int col)
        {
            Row = row;
            Col = col;
        }

    }
}
