// import java.util.Arrays;

class OutOfRangeException extends Exception {
    public OutOfRangeException(String message) {
        super(message);
    }
}

public class TrojkatPascala {

    private int[][] pascal;

    TrojkatPascala(int n) 
    {
        pascal = new int[n + 1][n + 1];

        for (int row = 0; row <= n; row++)
        {
            for (int col = 0; col <= row; col++) {
                if (col == 0 || col == row) {
                    pascal[row][col] = 1;
                } 
                else {
                    pascal[row][col] = pascal[row - 1][col - 1] + pascal[row - 1][col];
                }
            }
        }
    }

    public int[][] getArr() {
        return pascal;
    }

    public int get(int y, int x) throws OutOfRangeException {
        if (y < 0 || y >= pascal.length)
            throw new OutOfRangeException(y + " - wiersz spoza zakresu");
        if (x < 0 || x > y)
            throw new OutOfRangeException(x + " - kolumna spoza zakresu");
        return pascal[y][x];
    }
}