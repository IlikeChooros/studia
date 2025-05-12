import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

// Rectangle shape, can be either filled or just an outline
public class Rectangle implements SerializableShape {
    private double x0, y0, x1, y1;
    private Color fillColor;
    private Color strokeColor;
    private double strokeWidth;
    private double rotation;

    /**
     * Ensure the rectangle is drawn from the top-left corner
     *  to the bottom-right corner
     */
    private void prepareCoordinates() {
        
        double temp;
        if (x0 > x1) {
            temp = x0;
            x0 = x1;
            x1 = temp;
        }
        if (y0 > y1) {
            temp = y0;
            y0 = y1;
            y1 = temp;
        }
    }

    private void initialize(
        double startX, double startY, double endX, double endY,
        Color fillColor, Color strokeColor, double strokeWidth, double rotation
    ) {
        this.x0 = startX;
        this.y0 = startY;
        this.x1 = endX;
        this.y1 = endY;

        // Ensure the rectangle is drawn from the top-left corner
        // to the bottom-right corner
        prepareCoordinates();

        this.fillColor = fillColor;
        this.strokeColor = strokeColor;
        this.strokeWidth = strokeWidth;
        this.rotation = rotation;
    }

    /**
     * Create a rectangle with the given parameters, 
     * the coordinates do not have to be in any specific order.
     * 
     * @param x0 the starting x-coordinate
     * @param y0 the starting y-coordinate 
     * @param x1 ending x-coordinate
     * @param y1 ending y-coordinate
     * @param fillColor null if no fill is needed
     * @param strokeColor the color of the outline
     * @param strokeWidth the width of the outline
     * @param rotation the rotation of the rectangle
     */
    public Rectangle(
        double x0, double y0, double x1, double y1,
        Color fillColor, Color strokeColor, double strokeWidth,
        double rotation
    ) {
        initialize(x0, y0, x1, y1, fillColor, strokeColor, strokeWidth, rotation);
    }

    /**
     * Set the starting point of the rectangle.
     */
    @Override
    public void setStart(double x, double y) {
        this.x0 = x;
        this.y0 = y;
    }

    /**
     * Set the ending point of the rectangle.
     */
    @Override
    public void setEnd(double x, double y) {
        this.x1 = x;
        this.y1 = y;

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
            gc.fillRect(x0, y0, x1 - x0, y1 - y0);
        }

        // Draw the outline of the rectangle
        gc.setStroke(strokeColor);
        gc.setLineWidth(strokeWidth);
        gc.strokeRect(x0, y0, x1 - x0, y1 - y0);
    }
}