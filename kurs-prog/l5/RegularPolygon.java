import java.util.Arrays;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

// Base state for polygon
class PolygonState extends BaseShapeState {
    private static final long serialVersionUID = 401L;
    public double[] xPoints;
    public double[] yPoints;
    public int numPoints;

    PolygonState(double centerX, double centerY, int npoints, 
    Color fill, Color stroke, double strokeWidth, double rotation) 
    {
        super(centerX, centerY, fill, stroke, strokeWidth, rotation);
        this.xPoints    = new double[npoints];
        this.yPoints    = new double[npoints];
        this.numPoints  = npoints;

        for (int i = 0; i < npoints; i++) {
            this.xPoints[i] = centerX;
            this.yPoints[i] = centerY;
        }
    }

    public PolygonState(PolygonState other) {
        super(other);
        this.xPoints    = (double[])other.xPoints.clone();
        this.yPoints    = (double[])other.yPoints.clone();
        this.numPoints = other.numPoints;
    }

    // For debugging purposes
    @Override
    public String toString() {
        // Print all data
        StringBuilder sb = new StringBuilder();
        sb.append("{PolygonState: ");
        sb.append("center=(").append(centerX).append(", ").append(centerY).append("), ");
        sb.append("points=[");
        sb.append(Arrays.toString(xPoints)).append(", ");
        sb.append(Arrays.toString(yPoints)).append("], ");
        sb.append("numPoints=").append(numPoints).append(", ");
        sb.append("Fill: ").append(fillColor).append(", ");
        sb.append("strokeColor=").append(strokeColor).append(", ");
        sb.append("strokeWidth=").append(strokeWidth).append(", ");
        sb.append("Rotation: ").append(rotation).append("}");
        return sb.toString();
    }
}

// State object for abstract polygons, keeping information
// about the point's position, center coordinates etc.
class RegularPolygonState extends PolygonState {
    private static final long serialVersionUID = 400L;
    
    public double radius;

    public RegularPolygonState(
        double centerX, double centerY, double radius, 
        int npoints, double rotation, Color fill, Color stroke,
        double strokeWidth
    ) {
        super(centerX, centerY, npoints, fill, stroke, strokeWidth, rotation);
        this.radius     = radius;
    }

    public RegularPolygonState(RegularPolygonState other) {
        super(other);
        this.radius     = other.radius;
    }
}

abstract public class RegularPolygon extends TBaseShape<RegularPolygonState> {
    private static final long serialVersionUID = 40L;
    /**
     * Create a polygon points based on the # of points and the center coordinates
     */
    private void prepareCoordinates() {
        RegularPolygonState state = getLastState();
        double angleIncrement = 2 * Math.PI / state.numPoints;
        for (int i = 0; i < state.numPoints; i++) {
            // Make the polygon rotate clockwise
            // by subtracting the angle from 2 * PI
            state.xPoints[i] = state.centerX + state.radius * Math.cos(2 * Math.PI - i * angleIncrement + state.rotation);
            state.yPoints[i] = state.centerY + state.radius * Math.sin(2 * Math.PI - i * angleIncrement + state.rotation);
        }
    }

    /**
     * Create a polygon with specified number of vertices.
     * @param startX Center X coordinate
     * @param startY Center Y coordinate
     * @param endX End X coordinate (used to calculate radius)
     * @param endY End Y coordinate (used to calculate radius)
     * @param numPoints Number of vertices (3 for triangle, 4 for square, etc.)
     * @param fillColor Fill color (null if no fill is needed)
     * @param strokeColor Outline color
     * @param strokeWidth Width of the outline
     * @param rotation Rotation angle in radians
     */
    public RegularPolygon(
        double startX, double startY, double endX, double endY, int numPoints,
        Color fillColor, Color strokeColor, double strokeWidth, double rotation
    ) {
        RegularPolygonState state = new RegularPolygonState(
            startX, startY, endY, numPoints, rotation, 
            fillColor, strokeColor, strokeWidth
        );
        pushState(state);

        // Calculate the center and radius of the polygon
        setStart(startX, startY);
        setEnd(endX, endY);
    }

    /**
     * Set the center of the polygon.
     */
    @Override
    public void setStart(double x, double y) {
        RegularPolygonState state = getLastState();
        state.centerX = x;
        state.centerY = y;
    }

    /**
     * Set the radius of the polygon based on the end point.
     */
    @Override
    public void setEnd(double x, double y) {
        // r = sqrt((x0 - x1)^2 + (y0 - y1)^2), simple euclidean distance
        RegularPolygonState state = getLastState();
        state.radius = Math.sqrt(
            Math.pow(x - state.centerX, 2) + Math.pow(y - state.centerY, 2)
        );
        prepareCoordinates();
    }

    /**
     * Copy current state and push it as new one to the list of states
     */
    @Override
    public void copyState() {
        stateList.add(new RegularPolygonState(getLastState()));
    }

    /**
     * Adds to current state's posistion (dx, dy)
     */
    @Override
    public void move(double dx, double dy) {
        RegularPolygonState state =  getLastState();
        // Just add to every point dx and dy vectors
        for (int i = 0; i < state.numPoints; i++) {
            state.xPoints[i] += dx;
            state.yPoints[i] += dy;
        }
        state.centerX += dx;
        state.centerY += dy;
    }

    /**
     * Resize the polygon by given delta v
     */
    @Override
    public void resize(double dv) {
        RegularPolygonState state =  getLastState();
        state.radius += dv / 2;
        state.radius = Math.max(state.radius, 1);
        prepareCoordinates();
    }

    /**
     * Rotate the polygon by delta angle
     */
    @Override
    public void rotate(double da) {
        RegularPolygonState state =  getLastState();
        state.rotation += da;
        prepareCoordinates();
    }

    /**
     * Set the number of points for the polygon.
     */
    @Override
    public void draw(GraphicsContext gc) {
        RegularPolygonState state = getLastState();

        if (state.fillColor != null) {
            // Draw the filled polygon
            gc.setFill(state.fillColor);
            gc.fillPolygon(state.xPoints, state.yPoints, state.numPoints);
        }
        
        // Draw the outline of the polygon
        gc.setStroke(state.strokeColor);
        gc.setLineWidth(state.strokeWidth);
        gc.strokePolygon(state.xPoints, state.yPoints, state.numPoints);
    }

    /**
     * Check if a point is inside the polygon.
     * @param x X coordinate of the point
     * @param y Y coordinate of the point
     * @return true if the point is inside the polygon, false otherwise
     */
    @Override
    public boolean contains(double x, double y) {
        RegularPolygonState s = getLastState();
        return isContained(x, y, s.xPoints, s.yPoints);
    }
    
}
