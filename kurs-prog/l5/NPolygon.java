import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

abstract public class NPolygon extends BaseShape {
    private double[] xPoints;
    private double[] yPoints;
    private int numPoints;
    private double centerX, centerY;
    private double radius;

    /**
     * Create a polygon points based on the # of points and the center coordinates
     */
    private void prepareCoordinates() {
        double angleIncrement = 2 * Math.PI / numPoints;
        for (int i = 0; i < numPoints; i++) {
            // Make the polygon rotate clockwise
            // by subtracting the angle from 2 * PI
            xPoints[i] = centerX + radius * Math.cos(2 * Math.PI - i * angleIncrement + rotation);
            yPoints[i] = centerY + radius * Math.sin(2 * Math.PI - i * angleIncrement + rotation);
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
        super(startX, startY, endX, endY, fillColor, strokeColor, strokeWidth, rotation);
        this.numPoints = numPoints;
        this.xPoints = new double[numPoints];
        this.yPoints = new double[numPoints];

        // Calculate the center and radius of the polygon
        setStart(startX, startY);
        setEnd(endX, endY);
    }

    /**
     * Set the center of the polygon.
     */
    @Override
    public void setStart(double x, double y) {
        this.centerX = x;
        this.centerY = y;
    }

    /**
     * Set the radius of the polygon based on the end point.
     */
    @Override
    public void setEnd(double x, double y) {
        // r = sqrt((x0 - x1)^2 + (y0 - y1)^2), simple euclidean distance
        this.radius = Math.sqrt(
            Math.pow(x - centerX, 2) + Math.pow(y - centerY, 2)
        );
        prepareCoordinates();
    }

    /**
     * Set the number of points for the polygon.
     */
    @Override
    public void draw(GraphicsContext gc) {
        if (fillColor != null) {
            // Draw the filled polygon
            gc.setFill(fillColor);
            gc.fillPolygon(xPoints, yPoints, numPoints);
        }
        
        // Draw the outline of the polygon
        gc.setStroke(strokeColor);
        gc.setLineWidth(strokeWidth);
        gc.strokePolygon(xPoints, yPoints, numPoints);
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
        boolean inside = false;
        for (int i = 0, j = numPoints - 1; i < numPoints; j = i++) {
            if ((yPoints[i] > y) != (yPoints[j] > y) &&
                (x < (xPoints[j] - xPoints[i]) * (y - yPoints[i]) / (yPoints[j] - yPoints[i]) + xPoints[i])) {
                inside = !inside;
            }
        }
        return inside;
    }
    
}
