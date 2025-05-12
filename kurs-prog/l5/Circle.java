import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

// Circle shape, can be either filled or just an outline
public class Circle extends BaseShape {
    private double centerX, centerY, radius;

    public Circle(
        double x0, double y0, double x1, double y1,
        Color fillColor, Color strokeColor, double strokeWidth
    ) 
    {
        super(
            x0, y0, x1, y1, fillColor, strokeColor, strokeWidth, 0
        );
        // Calculate the center and radius of the circle
        setStart(x0, y0);
        setEnd(x1, y1);
    }

    /**
     * Set the center of the circle.
     */
    @Override
    public void setStart(double x, double y) {
        this.centerX = x;
        this.centerY = y;
    }

    /**
     * Set the radius of the circle based on the end point.
     */
    @Override
    public void setEnd(double x, double y) {
        // r = sqrt((x0 - x1)^2 + (y0 - y1)^2), simple euclidean distance
        this.radius = Math.sqrt(
            Math.pow(x - centerX, 2) + Math.pow(y - centerY, 2)
        );        
    }

    public void draw(GraphicsContext gc) {
        
        if (fillColor != null) {
            // Draw the filled circle
            gc.setFill(fillColor);
            gc.fillOval(centerX - radius, centerY - radius, radius * 2, radius * 2);
        }

        // Draw the outline of the circle
        gc.setStroke(strokeColor);
        gc.setLineWidth(strokeWidth);
        gc.strokeOval(centerX - radius, centerY - radius, radius * 2, radius * 2);
    }

    /**
     * Check if a point is inside the circle.
     * @param x X coordinate of the point
     * @param y Y coordinate of the point
     * @return true if the point is inside the circle, false otherwise
     */
    @Override
    public boolean contains(double x, double y) {
        // r = sqrt((x0 - x1)^2 + (y0 - y1)^2), simple euclidean distance
        double distance = Math.sqrt(
            Math.pow(x - centerX, 2) + Math.pow(y - centerY, 2)
        );
        
        return distance <= radius;
    }

    @Override
    public String getType() {
        return "Circle";
    }
}
