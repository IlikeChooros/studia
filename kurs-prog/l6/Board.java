import javafx.beans.binding.Bindings;
import javafx.geometry.Insets;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.RowConstraints;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.shape.Rectangle;

// Simple 2D board as a cell matrix
public class Board extends GridPane {

    static public final Color WOLF_COLOR = Color.SLATEGRAY;
    static public final Color RABBIT_COLOR = Color.WHITE;
    static public final Color EMPTY_COLOR = Color.DARKGREEN;

    // Using the rectangle 2D array for easy access
    private Rectangle[][] board;

    // Create a n x m board. initialCellSize is used only to set an initial preferred size.
    public Board(int numRows, int numCols, double initialCellSize) {
        super();

        // Set a preferred size so the initial window is reasonable.
        this.setPrefWidth(numCols * initialCellSize);
        this.setPrefHeight(numRows * initialCellSize);

        board = new Rectangle[numRows][numCols];

        // Create column constraints: each column gets an equal percentage.
        for (int c = 0; c < numCols; c++) {
            ColumnConstraints colConst = new ColumnConstraints();
            colConst.setPercentWidth(100.0 / numCols);
            getColumnConstraints().add(colConst);
        }

        // Create row constraints: each row gets an equal percentage.
        for (int r = 0; r < numRows; r++) {
            RowConstraints rowConst = new RowConstraints();
            rowConst.setPercentHeight(100.0 / numRows);
            getRowConstraints().add(rowConst);
        }

        // Add cells to the grid
        for (int r = 0; r < numRows; r++) {
            for (int c = 0; c < numCols; c++) {
                // Create a container to make the cell resizable.
                StackPane cellContainer = new StackPane();
                // Create the rectangle without specifying width and height.
                Rectangle cell = new Rectangle();
                cell.setFill(EMPTY_COLOR);

                // Bind the rectangle's size to the container's size
                cell.widthProperty().bind(cellContainer.widthProperty());
                cell.heightProperty().bind(cellContainer.heightProperty());

                // Add the rectangle to the container.
                cellContainer.getChildren().add(cell);
                // Save the reference if needed later.
                board[r][c] = cell;
                // Add the container to the grid at (c, r)
                add(cellContainer, c, r);
            }
        }
    }

    /**
     * Return the color at given coordinates
     */
    public synchronized Paint getColor(int x, int y) {
        return (Paint)board[y][x].getFill();
    }

    /**
     * Set the color on the (x, y) coordinates
    */ 
    public synchronized void setColor(int x, int y, Paint color) {
        board[y][x].setFill(color);
    }
}
