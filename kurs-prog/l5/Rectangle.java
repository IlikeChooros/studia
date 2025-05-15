import javafx.geometry.Point2D;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

// Rectangle shape, can be either filled or just an outline
public class Rectangle extends BaseShape {
    private static final long serialVersionUID = 60L;

    /**
     * Ensure the rectangle is drawn from the top-left corner
     *  to the bottom-right corner
     */
    private void prepareCoordinates() {
        ShapeState state = getLastState();
        double temp;
        if (state.startX > state.endX) {
            temp = state.startX;
            state.startX = state.endX;
            state.endX = temp;
        }
        if (state.startY > state.endY) {
            temp = state.startY;
            state.startY = state.endY;
            state.endY = temp;
        }
    }

    /**
     * Create a rectangle with the given parameters, 
     * the coordinates do not have to be in any specific order.
     * 
     * @param startX the starting x-coordinate
     * @param startY the starting y-coordinate 
     * @param endX ending x-coordinate
     * @param endY ending y-coordinate
     * @param fillColor null if no fill is needed
     * @param strokeColor the color of the outline
     * @param strokeWidth the width of the outline
     * @param rotation the rotation of the rectangle
     */
    public Rectangle(
        double startX, double startY, double endX, double endY,
        Color fillColor, Color strokeColor, double strokeWidth,
        double rotation
    ) {
        super(
            startX, startY, endX, endY, fillColor, strokeColor, strokeWidth, rotation
        );
        // Ensure the rectangle is drawn from the top-left corner
        // to the bottom-right corner
        prepareCoordinates();
    }


    /**
     * Set the ending point of the rectangle.
     */
    @Override
    public void setEnd(double x, double y) {
        super.setEnd(x, y);

        // Ensure the rectangle is drawn from the top-left corner
        // to the bottom-right corner
        prepareCoordinates();
    }


    /**
     * Draw the rectangle on the given GraphicsContext.
     * @param gc the GraphicsContext to draw on
     */
    @Override
    public void draw(GraphicsContext gc) {
        ShapeState state = getLastState();
        if (state.fillColor != null) {
            // Draw the filled rectangle
            gc.setFill(state.fillColor);
            gc.fillRect(state.startX, state.startY, state.endX - state.startX, state.endY - state.startY);
        }

        // Draw the outline of the rectangle
        gc.setStroke(state.strokeColor);
        gc.setLineWidth(state.strokeWidth);
        gc.strokeRect(state.startX, state.startY, state.endX - state.startX, state.endY - state.startY);
    }

    /**
     * Check if the given point is inside the rectangle.
     * @param x the x-coordinate of the point
     * @param y the y-coordinate of the point
     * @return true if the point is inside the rectangle, false otherwise
     */
    @Override
    public boolean contains(double x, double y) {
        ShapeState state = getLastState();
        return (x >= state.startX && x <= state.endX && y >= state.startY && y <= state.endY);
    }

    @Override
    public void rotate(double da) {
        ShapeState state = getLastState();

        
    }

    @Override
    public Point2D getCenter() {
        ShapeState state = getLastState();
        return new Point2D((state.startX + state.endX) / 2, (state.startY + state.endY) / 2);
    }

    /**
     * Get the type of the shape.
     * @return the type of the shape
     */
    @Override
    public String getType() {
        return "Rectangle";
    }
}