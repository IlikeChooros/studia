import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;

// Simple 2D board as a cell matrix
public class Board extends GridPane {

    static public final Color WOLF_COLOR = Color.SLATEGRAY;
    static public final Color RABBIT_COLOR = Color.WHITE;
    static public final Color EMPTY_COLOR = Color.DARKGREEN;

    // Using the rectangle 2D array for easy access
    private Rectangle[][] board;

    // Create a n x m (row x col) 2d matrix board, with
    // given cellsize
    public Board(int n, int m, double cellSize) {
        super();

        board = new Rectangle[n][m];
        for(int r = 0; r < n; r++) {
            for (int c = 0; c < m; c++) {
                // Create the rectangle object
                Rectangle cell = new Rectangle(cellSize, cellSize);
                cell.setFill(EMPTY_COLOR);
                cell.setStroke(Color.BLACK);
                cell.setStrokeWidth(1);
                board[r][c] = cell; // set the object
                add(cell, c, r); // add it to the layout
            }
        }
    }

    // Set the color on the (x, y) coordinates
    public void setColor(int x, int y, Color color) {
        board[y][x].setFill(color);
    }
}
