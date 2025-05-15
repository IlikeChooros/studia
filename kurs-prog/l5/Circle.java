import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

// Class for keeping circle's state, holds center x,y and radius
class CircleState extends BaseShapeState {
    private static final long serialVersionUID = 300L;
    public double radius;

    public CircleState(
        double centerX, double centerY, double radius,
        Color fillColor, Color strokeColor, double strokeWidth
    )
    {
        super(centerX, centerY, fillColor, strokeColor, strokeWidth, 0);
        this.radius  = radius;
    }

    // Copy constructor
    public CircleState(CircleState other) {
        super(other);
        this.radius  = other.radius;
    }
};

// Circle shape, can be either filled or just an outline
public class Circle extends TBaseShape<CircleState> {
    private static final long serialVersionUID = 10L;

    private static double getRadius(double cx, double cy, double x, double y) {
        return Math.sqrt(Math.pow(cx - x, 2) + Math.pow(cy - y, 2));
    }

    public Circle(
        double x0, double y0, double x1, double y1,
        Color fillColor, Color strokeColor, double strokeWidth
    ) 
    {
        pushState(new CircleState(x0, y0, getRadius(x0, y0, x1, y1), fillColor, strokeColor, strokeWidth));
    }

    /**
     * Set the center of the circle.
     */
    @Override
    public void setStart(double x, double y) {
        CircleState s = getLastState();
        s.centerX = x;
        s.centerY = y;
    }

    /**
     * Set the radius of the circle based on the end point.
     */
    @Override
    public void setEnd(double x, double y) {
        CircleState s = getLastState();
        s.radius = getRadius(s.centerX, s.centerY, x, y);
    }

    /**
     * Move the center of the circle by (dx, dy) vector
     */
    @Override
    public void move(double dx, double dy) {
        CircleState s = getLastState();
        s.centerX += dx;
        s.centerY += dy;
    }

    /**
     * Rotation does not affect the cricle duh
     */
    @Override
    public void rotate(double da) {}

    /**
     * Resize the circle (change the radius by given delta v)
     */
    @Override
    public void resize(double dv) {
        CircleState s = getLastState();
        s.radius += dv / 2;
        s.radius = Math.max(1, s.radius);
    }

    /**
     * Create a copy of current state and push it to the state list (as new current state)
     */
    @Override
    public void copyState() {
        stateList.add(new CircleState(getLastState()));
    }    

    @Override
    public void draw(GraphicsContext gc) {
        CircleState s = getLastState();
        
        if (s.fillColor != null) {
            // Draw the filled circle
            gc.setFill(s.fillColor);
            gc.fillOval(s.centerX - s.radius, s.centerY - s.radius, s.radius * 2, s.radius * 2);
        }

        // Draw the outline of the circle
        gc.setStroke(s.strokeColor);
        gc.setLineWidth(s.strokeWidth);
        gc.strokeOval(s.centerX - s.radius, s.centerY - s.radius, s.radius * 2, s.radius * 2);
    }

    /**
     * Check if a point is inside the circle.
     * @param x X coordinate of the point
     * @param y Y coordinate of the point
     * @return true if the point is inside the circle, false otherwise
     */
    @Override
    public boolean contains(double x, double y) {
        CircleState s = getLastState();
        // r = sqrt((x0 - x1)^2 + (y0 - y1)^2), simple euclidean distance
        double distance = Math.sqrt(
            Math.pow(x - s.centerX, 2) + Math.pow(y - s.centerY, 2)
        );
        
        return distance <= s.radius;
    }

    @Override
    public String getType() {
        return "Circle";
    }
}
