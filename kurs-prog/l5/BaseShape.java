import javafx.geometry.Point2D;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import java.io.Serializable;

import java.util.ArrayList;
import java.util.List;

// To modify the shapes on the canvas, we need to store the shapes as
// objects. These objects should implement a drawing method, selection,
// rotation, and allow for resizing (with scroll) and moving.

// Additionally, these objects should be serializable so that we can
// save the canvas as well as the shapes into a file

interface IShape {
    /**
     * Draw a shape into a given canvas
     * @param gc
     */
    public void draw(GraphicsContext gc);

    /**
     * Set starting position of the object (depends on the figure)
     */
    public void setStart(double x, double y);

    /**
     * Set the ending position of the shape (in rectangle that's the ending point, in
     * circle, it calculates the radius)
     */
    public void setEnd(double x, double y);

    /**
     * Check if given point is contained within a given shape
     * @return true if it is, false otherwise
     */
    public boolean contains(double x, double y);

    /**
     * Move the shape by given delta (x, y) vector
     */
    public void move(double deltaX, double deltaY);

    /**
     * Change the size by given delta x value
     */
    public void resize(double dv);

    /**
     * Copy current state of the shape, and push it to the
     * state list as a new one
     */
    public void copyState();

    /**
     * Check if the shape has any history of modifications
     * @return true if it was modified (thus it's state can be restored), false otherwise
     */
    public boolean hasModifiedStates();

    /**
     * Restore the previous state of the shape
     */
    public void restoreState();

    /**
     * Rotate the shape by given angle (in radians)
     */
    public void rotate(double da);

    /**
     * Get rotation angle of the shape
     */
    public double getRotation();

    /**
     * Get the center coordinates of the shape
     */
    public Point2D getCenter();

    /**
     * Get the name of the shape
     * @return Human redable string of the shape
     */
    public String getType(); 
}

/**
 * Abstract, base class for all shapes, implements SerializableShape and Cloneable
 */
abstract class BShape implements IShape, Serializable, Cloneable {
    private static final long serialVersionUID = 1L;
    public int id = 0;

    /*
     * Rotate the point by given origin and angle
     */
    public static Point2D rotatePoint(Point2D p, Point2D origin, double da) {
        double x, y;
        double cosA = Math.cos(da), sinA = Math.sin(da);

        double nx = p.getX() - origin.getX(),
               ny = p.getY() - origin.getY();

        x = origin.getX() + nx * cosA - ny * sinA;
        y = origin.getY() + ny * cosA + nx * sinA;

        return new Point2D(x, y);
    }

    /**
     * Check if the given point (x, y) is contained within the polygon, defined as
     * array of vertices (px, py)
     * @return true if the point is inside the polygon, false otherwise
     */
    static public boolean isContained(double x, double y, double[] px, double[] py) {
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
        for (int i = 0, j = px.length - 1; i < px.length; j = i++) {
            if ((py[i] > y) != (py[j] > y) 
                && (x < (px[j] - px[i]) * (y - py[i]) 
                        / (py[j] - py[i]) + px[i])) {
                inside = !inside;
            }
        }
        return inside;
    }

    /**
     * Check if given point p is on the line segment
     * (start, end) with given strokeWidth
     * @param start Start point of the line
     * @param end End point of the line
     * @param p Point to check
     * @param strokeWidth Width of the line
     * @return true if the point is on the line segment, false otherwise
     */
    public static boolean containsInLine(
        Point2D start, Point2D end, Point2D p, double strokeWidth) 
    {
        double minX = Math.min(start.getX(), end.getX());
        double maxX = Math.max(start.getX(), end.getX());
        double minY = Math.min(start.getY(), end.getY());
        double maxY = Math.max(start.getY(), end.getY());
        
        if (p.getX() < minX || p.getX() > maxX || p.getY() < minY || p.getY() > maxY) {
            return false;
        }

        // Calculate the distance from the point to the line segment
        double distance = Math.abs(
            (end.getY() - start.getY()) * p.getX() - (end.getX() - start.getX()) * p.getY() + end.getX() * start.getY() - end.getY() * start.getX()
        ) / Math.sqrt(Math.pow(end.getY() - start.getY(), 2) + Math.pow(end.getX() - start.getX(), 2));

        return distance <= strokeWidth / 2;
    }
}

// Base class for shapes, can be extended for specific shapes
abstract class TBaseShape<T> extends BShape {
    private static final long serialVersionUID = 2L;
    protected List<T> stateList = new ArrayList<T>();

    /**
     * Push new state to the list
     */
    protected void pushState(T state)
    {
        stateList.add(state);
    }

    /**
     * Get the (stateList.size() - 1) element from stateList
     * @return ShapeState object or null
     */
    protected T getLastState() {
        return stateList.get(stateList.size() - 1);
    }


    /**
     * Check if the shape has any history of modifications
     * (so that we can restore the previous states)
     */
    public boolean hasModifiedStates() {
        return stateList.size() >= 2;
    }

    /**
     * Restore the previous state (meaning there should be at least 2 or more objects
     * in state list)
     */
    public void restoreState() {
        if (stateList.size() >= 2) {
            stateList.remove(stateList.size() - 1);
        }
    }
}

// Base class for shapes, can be extended for specific shapes
abstract class BaseShape extends TBaseShape<ShapeState> {
    private static final long serialVersionUID = 3L;
    
    public BaseShape(
        double startX, double startY, double endX, double endY,
        Color fillColor, Color strokeColor, double strokeWidth,
        double rotation
    ) {
        pushState(new ShapeState(
                startX, startY, endX, endY, fillColor, 
                strokeColor, strokeWidth, rotation));
    }

    @Override
    public double getRotation() {
        return getLastState().rotation;
    }

    /**
     * Copies current last state, and adds it onto the list
     */
    @Override
    public void copyState() {
        stateList.add(new ShapeState(getLastState()));
    }

    /**
     * Applies move vector the to interal points
     */
    @Override
    public void move(double deltaX, double deltaY) {
        ShapeState s = getLastState();
        s.startX += deltaX;
        s.endX   += deltaX;
        s.startY += deltaY;
        s.endY   += deltaY;
    }

    /**
     * Resizes the shape, by given delta value
     */
    @Override
    public void resize(double dv) {
        ShapeState s = getLastState();
        
        // dx^2 + dy^2 = dv^2
        // dy / dx = alpha -> dy^2 = dx^2 * alpha^2
        // alpha = height / width  = abs(endY - startY) / abs(endX - startX)
        // dx^2 = dv^2 / (1 + alpha^2)
        // dx   = dv / sqrt(1 + alpha^2)
        // dy   = dx * alpha
        double alpha = Math.abs(s.endY - s.startY) / Math.abs(s.endX - s.startX);
        double dx = dv / Math.sqrt(1 + Math.pow(alpha, 2));
        double dy = alpha * dx;

        if (Math.abs(s.startX - s.endX) > -2.5 * dx &&
            Math.abs(s.startY - s.endY) > -2.5 * dy
        )
        {
            s.startX -= dx;
            s.endX   += dx;
            s.startY -= dy;
            s.endY   += dy;
        }        
    }

    /**
     * Set the start coordinates, by default sets the state object's
     * `startX` and `startY` accordingly
     */
    @Override
    public void setStart(double x, double y) {
        ShapeState state = getLastState();
        state.startX = x;
        state.startY = y;
    }

    /**
     * Set the end coordinates (when mouse is released), by default sets the 
     * state object's `endX` and `endY` fileds
     */
    @Override
    public void setEnd(double x, double y) {
        ShapeState state = getLastState();
        state.endX = x;
        state.endY = y;
    }

    @Override
    public void rotate(double da) {
        ShapeState s = getLastState();
        double [] xpoints = {s.startX, s.endX, s.startX, s.endX};
        double [] ypoints = {s.startY, s.startY, s.endY, s.endY};
        Point2D origin = new Point2D((s.startX + s.endX) / 2, (s.startY + s.endY) / 2);

        for (int i = 0; i < xpoints.length; i++) {
            Point2D p = rotatePoint(new Point2D(xpoints[i], ypoints[i]), origin, da);

            xpoints[i] = p.getX();
            ypoints[i] = p.getY();
        }
    }

    
}