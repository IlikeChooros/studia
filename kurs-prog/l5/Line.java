import javafx.geometry.Point2D;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;


/**
 * Represents a line (well duh)
 */
public class Line extends BaseShape {
    private static final long serialVersionUID = 30L;

    public Line(
        double startX, double startY, double endX, double endY,
        Color strokeColor, double strokeWidth
    ) {
        super(
            startX, startY, endX, endY, null, strokeColor, strokeWidth, 0
        );
    }

    @Override
    public void draw(GraphicsContext gc) {
        ShapeState state = getLastState();
        gc.setStroke(state.strokeColor);
        gc.setLineWidth(state.strokeWidth);
        gc.strokeLine(state.startX, state.startY, state.endX, state.endY);
    }

    @Override
    public void resize(double dv) {
        ShapeState s = getLastState();
        s.strokeWidth += (dv > 0 ? 2 : -2);
        s.strokeWidth = Math.max(1, s.strokeWidth);
    }

    @Override
    public void rotate(double da) {
        ShapeState s = getLastState();
        Point2D origin = new Point2D((s.startX + s.endX) / 2, (s.startY + s.endY) / 2);
        Point2D start = new Point2D(s.startX, s.startY);
        Point2D end = new Point2D(s.endX, s.endY);

        Point2D rotStart = rotatePoint(start, origin, da);
        Point2D rotEnd   = rotatePoint(end, origin, da);

        s.startX = rotStart.getX();
        s.startY = rotStart.getX();
        s.endX = rotEnd.getX();
        s.endY = rotEnd.getX();
    }

    /**
     * Check if given point stands on the line i.e.
     * given dist = abs(Ax + Bx + C) / sqrt(A^2 + B^2)
     * @return dist <= strokeWidth / 2
     */
    @Override
    public boolean contains(double x, double y) {
        ShapeState s = getLastState();
        return containsInLine(
            new Point2D(s.startX, s.startY), 
            new Point2D(s.endX, s.endY), 
            new Point2D(x, y), s.strokeWidth
        );
        // Check if the point is outside the bounding box of the line
        // ShapeState state = getLastState();

        // double minX = Math.min(state.startX, state.endX);
        // double maxX = Math.max(state.startX, state.endX);
        // double minY = Math.min(state.startY, state.endY);
        // double maxY = Math.max(state.startY, state.endY);
        
        // if (x < minX || x > maxX || y < minY || y > maxY) {
        //     return false;
        // }

        // // Calculate the distance from the point to the line segment
        // double distance = Math.abs(
        //     (state.endY - state.startY) * x - (state.endX - state.startX) * y + state.endX * state.startY - state.endY * state.startX
        // ) / Math.sqrt(Math.pow(state.endY - state.startY, 2) + Math.pow(state.endX - state.startX, 2));

        // return distance <= state.strokeWidth / 2;
    }

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

    @Override
    public String getType() {
        return "Line";
    }
}