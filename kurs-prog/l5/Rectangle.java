import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

// Rectangle shape, can be either filled or just an outline
public class Rectangle extends BaseShape {

    /**
     * Ensure the rectangle is drawn from the top-left corner
     *  to the bottom-right corner
     */
    private void prepareCoordinates() {
        
        double temp;
        if (startX > endX) {
            temp = startX;
            startX = endX;
            endX = temp;
        }
        if (startY > endY) {
            temp = startY;
            startY = endY;
            endY = temp;
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
     * Set the starting point of the rectangle.
     */
    @Override
    public void setStart(double x, double y) {
        this.startX = x;
        this.startY = y;
    }

    /**
     * Set the ending point of the rectangle.
     */
    @Override
    public void setEnd(double x, double y) {
        this.endX = x;
        this.endY = y;

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
        
        if (fillColor != null) {
            // Draw the filled rectangle
            gc.setFill(fillColor);
            gc.fillRect(startX, startY, endX - startX, endY - startY);
        }

        // Draw the outline of the rectangle
        gc.setStroke(strokeColor);
        gc.setLineWidth(strokeWidth);
        gc.strokeRect(startX, startY, endX - startX, endY - startY);
    }

    /**
     * Check if the given point is inside the rectangle.
     * @param x the x-coordinate of the point
     * @param y the y-coordinate of the point
     * @return true if the point is inside the rectangle, false otherwise
     */
    @Override
    public boolean contains(double x, double y) {
        return (x >= startX && x <= endX && y >= startY && y <= endY);
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