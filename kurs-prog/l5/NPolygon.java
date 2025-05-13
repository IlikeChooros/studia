import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

// State object for abstract polygons, keeping information
// about the point's position, center coordinates etc.
class PolygonState extends BaseShapeState {
    private static final long serialVersionUID = 400L;
    public double[] xPoints;
    public double[] yPoints;
    public int numPoints;
    public double centerX, centerY;
    public double radius;

    public PolygonState(
        double centerX, double centerY, double radius, 
        int npoints, double rotation, Color fill, Color stroke,
        double strokeWidth
    ) {
        super(fill, stroke, strokeWidth, rotation);
        this.xPoints    = new double[npoints];
        this.yPoints    = new double[npoints];
        this.centerX    = centerX;
        this.centerY    = centerY;
        this.radius     = radius;
        this.numPoints  = npoints;
    }

    public PolygonState(PolygonState other) {
        super(other);
        this.numPoints = other.numPoints;
        this.centerX    = other.centerX;
        this.centerY    = other.centerY;
        this.radius     = other.radius;
        this.xPoints    = (double[])other.xPoints.clone();
        this.yPoints    = (double[])other.yPoints.clone();
    }
}

abstract public class NPolygon extends TBaseShape<PolygonState> {
    private static final long serialVersionUID = 40L;
    /**
     * Create a polygon points based on the # of points and the center coordinates
     */
    private void prepareCoordinates() {
        PolygonState state = getLastState();
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
    public NPolygon(
        double startX, double startY, double endX, double endY, int numPoints,
        Color fillColor, Color strokeColor, double strokeWidth, double rotation
    ) {
        PolygonState state = new PolygonState(
            startX, startY, endY, numPoints, rotation, 
            fillColor, strokeColor, strokeWidth
        );
        pushState(state);

        state.numPoints = numPoints;
        state.xPoints = new double[numPoints];
        state.yPoints = new double[numPoints];

        // Calculate the center and radius of the polygon
        setStart(startX, startY);
        setEnd(endX, endY);
    }

    /**
     * Set the center of the polygon.
     */
    @Override
    public void setStart(double x, double y) {
        PolygonState state = getLastState();
        state.centerX = x;
        state.centerY = y;
    }

    /**
     * Set the radius of the polygon based on the end point.
     */
    @Override
    public void setEnd(double x, double y) {
        // r = sqrt((x0 - x1)^2 + (y0 - y1)^2), simple euclidean distance
        PolygonState state = getLastState();
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
        stateList.add(new PolygonState(getLastState()));
    }

    /**
     * Adds to current state's posistion (dx, dy)
     */
    @Override
    public void move(double dx, double dy) {
        PolygonState state =  getLastState();
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
        PolygonState state =  getLastState();
        state.radius += dv / 2;
        state.radius = Math.max(state.radius, 1);
        prepareCoordinates();
    }

    /**
     * Set the number of points for the polygon.
     */
    @Override
    public void draw(GraphicsContext gc) {
        PolygonState state = getLastState();

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
        // Author: W. Randolph Franklin (although that was written in C)
        // https://wrfranklin.org/Research/Short_Notes/pnpoly.html

        /*
         int i, j, c = 0;
            for (i = 0, j = nvert-1; i < nvert; j = i++) {
                if ( ((verty[i]>testy) != (verty[j]>testy)) &&
                (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
                c = !c;
            }
            return c;
         */
        PolygonState state = getLastState();
        boolean inside = false;
        for (int i = 0, j = state.numPoints - 1; i < state.numPoints; j = i++) {
            if ((state.yPoints[i] > y) != (state.yPoints[j] > y) 
                && (x < (state.xPoints[j] - state.xPoints[i]) * (y - state.yPoints[i]) 
                        / (state.yPoints[j] - state.yPoints[i]) + state.xPoints[i])) {
                inside = !inside;
            }
        }
        return inside;
    }
    
}
