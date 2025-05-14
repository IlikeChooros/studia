import javafx.geometry.Point2D;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import java.util.*;

public class CustomPolygon extends TBaseShape<PolygonState> {
    
    public CustomPolygon(double startX, double startY, 
        Color fill, Color outline, double strokeWidth) {
        pushState(new PolygonState(
            0, fill, outline, strokeWidth, 0
        ));

        addPoint(startX, startY);
    }

    /**
     * Push new state to the state list, and append 
     * @param x
     * @param y
     */
    public void addPoint(double x, double y) {
        copyState();
        PolygonState s = getLastState();

        s.numPoints += 1;
        
        double[] px = Arrays.copyOf(s.xPoints, s.numPoints);
        double[] py = Arrays.copyOf(s.yPoints, s.numPoints);
        px[px.length - 1] = x;
        py[py.length - 1] = y;

            System.out.println(e.isShiftDown());
        s.xPoints = px;
        s.yPoints = py;
    }

    @Override
    public void  setStart(double x, double y) {
        PolygonState s = getLastState();
        s.xPoints[0] = x;
        s.yPoints[0] = y;
    }

    @Override
    public void setEnd(double x, double y) {
        PolygonState s = getLastState();
        s.xPoints[s.numPoints - 1] = x;
        s.yPoints[s.numPoints - 1] = y;
    }

    @Override
    public void draw(GraphicsContext g) {
        PolygonState s = getLastState();
        if (s.numPoints == 1)
            return;
        if (s.numPoints == 2) {
            g.draw(s.xPoints[0], s.yPoints[0], 
                s.xPoints[1], s.yPoints[1], s.strokeWidth);
            return;
        }

        g.drawPolygon(s.xPoints, s.yPoints, s.numPoints, 
            s.fillColor, s.outlineColor, s.strokeWidth);
    }


    @Override
    public boolean contains(double x, double y) 
    {
        PolygonState s = getLastState();
        if (s.numPoints == 1)
            return false;
        if (s.numPoints == 2) {
            return Line.containsInLine(
                new Point2D(s.xPoints[0], s.yPoints[0]), 
                new Point2D(s.xPoints[1], s.yPoints[1]), 
                new Point2D(x, y), s.strokeWidth);
        }

        return NPolygon.isContained(x, y, s.xPoints, s.yPoints);
    }

    @Override
    public String getType() {
        return "Custom Polygon";
    }
}
