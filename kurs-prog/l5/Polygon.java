import javafx.geometry.Point2D;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import java.util.*;

public class Polygon extends TBaseShape<PolygonState> {

    public static final long serialVersionUID = 50L;
    public static final double VERTEX_RADIUS = 2.5;

    private void calculateCenter(PolygonState s) {
        // Update the center coordinates (update the average)
        s.centerX = s.xPoints[0];
        s.centerY = s.yPoints[0];

        // Recaulculate the center coordinates
        for (int i = 1; i < s.numPoints; i++) {
            s.centerX += (s.xPoints[i] - s.centerX) / (i + 1);
            s.centerY += (s.yPoints[i] - s.centerY) / (i + 1);
        }
    }
    
    public Polygon(double startX, double startY, 
        Color fill, Color outline, double strokeWidth) {
        pushState(new PolygonState(
            startX, startY, 1, fill, outline, strokeWidth, 0
        ));
    }

    @Override
    public void copyState() {
        stateList.add(new PolygonState(getLastState()));
    }

    /**
     * Show the vertices of the polygon
     */
    public void showVertices(GraphicsContext g) {
        PolygonState s = getLastState();
        g.setFill(null);
        g.setStroke(Color.BLACK);
        g.setLineWidth(1);

        for (int i = 0; i < s.numPoints; i++) {
            // It draws the oval from the top-left corner
            g.strokeOval(s.xPoints[i] - 2, s.yPoints[i] - 2, 4, 4);
        }

        // Draw the center point
        g.setFill(Color.RED);
        g.fillOval(s.centerX - 2, s.centerY - 2, 4, 4);
        g.setFill(null);
    }

    /**
     * Add a point to the polygon
     */
    public void addPoint(double x, double y) {
        PolygonState s = getLastState();

        s.numPoints += 1;
        
        double[] px = Arrays.copyOf(s.xPoints, s.numPoints);
        double[] py = Arrays.copyOf(s.yPoints, s.numPoints);
        px[px.length - 1] = x;
        py[py.length - 1] = y;

        s.xPoints = px;
        s.yPoints = py;
        calculateCenter(s);
    }

    public void rotate(double angle) {
        // Use the rotatePoint method from the base class
        PolygonState s = getLastState();
        double[] xpoints = Arrays.copyOf(s.xPoints, s.numPoints);
        double[] ypoints = Arrays.copyOf(s.yPoints, s.numPoints);
        Point2D origin = new Point2D(s.centerX, s.centerY);

        // Rotate each point around the center
        for (int i = 0; i < xpoints.length; i++) {
            Point2D p = BaseShape.rotatePoint(new Point2D(xpoints[i], ypoints[i]), origin, angle);
            xpoints[i] = p.getX();
            ypoints[i] = p.getY();
        }

        // Update the state with the new coordinates and rotation
        s.xPoints = xpoints;
        s.yPoints = ypoints;
        s.rotation += angle;

        // Normalize the rotation angle
        s.rotation = (s.rotation + Math.PI * 2) % (Math.PI * 2);
    }

    /**
     * Get the current rotation angle
     */
    @Override
    public double getRotation() {
        return getLastState().rotation;
    }

    /**
     * Get the center coordinates of the polygon
     */
    @Override
    public Point2D getCenter() {
        PolygonState s = getLastState();
        return new Point2D(s.centerX, s.centerY);
    }

    /**
     * Resize the polygon by given delta v
     */
    @Override
    public void resize(double dv) {
        // Each point should be moved by the same amount
        // from the center, so we can calculate the distance
        // from the center to each point and then move
        // each point by (dx, dy) vector, so that dx^2 + dy^2 = dv^2

        PolygonState s = getLastState();

        // dx^2 + dy^2 = dv^2
        // dy / dx = alpha -> dy^2 = dx^2 * alpha^2
        // alpha = height / width  = abs(endY - startY) / abs(endX - startX)
        // dx^2 = dv^2 / (1 + alpha^2)
        // dx   = dv / sqrt(1 + alpha^2)
        // dy   = dx * alpha
        for (int i = 0; i < s.numPoints; i++) {
            double alpha = Math.abs(s.centerY - s.yPoints[i]) / Math.abs(s.centerX - s.xPoints[i]);
            double dx = dv / Math.sqrt(1 + Math.pow(alpha, 2));
            double dy = alpha * dx;

            // System.out.println("alpha: " + alpha + " dx: " + dx + " dy: " + dy);

            if (Math.abs(s.centerX - s.xPoints[i]) > -2.5 * dx &&
                Math.abs(s.centerY - s.yPoints[i]) > -2.5 * dy
            )
            {
                // Check the direction of the point
                if (s.xPoints[i] > s.centerX) {
                    s.xPoints[i] += dx;
                } else {
                    s.xPoints[i] -= dx;
                }
                if (s.yPoints[i] > s.centerY) {
                    s.yPoints[i] += dy;
                } else {
                    s.yPoints[i] -= dy;
                }
            }
        }
    }

    /**
     * Move the polygon by given dx, dy
     */
    @Override
    public void move(double dx, double dy) {
        PolygonState s = getLastState();
        // Just add to every point dx and dy vectors
        for (int i = 0; i < s.numPoints; i++) {
            s.xPoints[i] += dx;
            s.yPoints[i] += dy;
        }
        s.centerX += dx;
        s.centerY += dy;
    }

    /**
     * Check if the given point is a vertex of the polygon (within 
     * VERTEX_RADIUS distance)
     */
    public boolean isVertex(double x, double y) {
        PolygonState s = getLastState();
        for (int i = 0; i < s.numPoints; i++) {
            if (Math.abs(s.xPoints[i] - x) < VERTEX_RADIUS &&
                Math.abs(s.yPoints[i] - y) < VERTEX_RADIUS)
            {
                return true;
            }
        }
        return false;
    }

    @Override
    public void  setStart(double x, double y) {
        PolygonState s = getLastState();
        s.xPoints[0] = x;
        s.yPoints[0] = y;

        // Update the center coordinates (update the average)
        calculateCenter(s);
    }

    @Override
    public void setEnd(double x, double y) {
        PolygonState s = getLastState();
        s.xPoints[s.numPoints - 1] = x;
        s.yPoints[s.numPoints - 1] = y;

        calculateCenter(s);
    }

    /**
     * Draw the polygon on the canvas
     */
    @Override
    public void draw(GraphicsContext g) {
        PolygonState s = getLastState();

        System.out.println("DRAWING: " + s.numPoints + " points" +
            " center: (" + s.centerX + ", " + s.centerY + ") " +
            Arrays.toString(s.xPoints) + " " +
            Arrays.toString(s.yPoints));

        g.setFill(s.fillColor);
        g.setStroke(s.strokeColor);
        g.setLineWidth(s.strokeWidth);

        // If there is only one point, do not draw anything
        if (s.numPoints == 1)
        {
            
            return;
        }

        // draw the line if there are only two points
        if (s.numPoints == 2) {
            g.strokeLine(s.xPoints[0], s.yPoints[0], 
                s.xPoints[1], s.yPoints[1]);
            return;
        }

        g.fillPolygon(s.xPoints, s.yPoints, s.numPoints);
        g.strokePolygon(s.xPoints, s.yPoints, s.numPoints);

        g.strokeOval(s.centerX - VERTEX_RADIUS, s.centerY - VERTEX_RADIUS, 
                VERTEX_RADIUS*2, VERTEX_RADIUS*2);
    }

    /**
     * Check if a point is inside the polygon.
     * @param x X coordinate of the point
     * @param y Y coordinate of the point
     * @return true if the point is inside the polygon, false otherwise
     */
    @Override
    public boolean contains(double x, double y) 
    {
        PolygonState s = getLastState();
        if (s.numPoints == 1)
            return false;
        if (s.numPoints == 2) {
            return containsInLine(
                new Point2D(s.xPoints[0], s.yPoints[0]), 
                new Point2D(s.xPoints[1], s.yPoints[1]), 
                new Point2D(x, y), s.strokeWidth);
        }

        return isContained(x, y, s.xPoints, s.yPoints);
    }

    @Override
    public String getType() {
        return "Custom Polygon";
    }
}
