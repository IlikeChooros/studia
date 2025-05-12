import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;


public class Line extends BaseShape {

    public Line(
        double startX, double startY, double endX, double endY,
        Color strokeColor, double strokeWidth
    ) {
        super(
            startX, startY, endX, endY, null, strokeColor, strokeWidth, 0
        );
    }

    /**
     * Set the starting point of the line.
     */
    @Override
    public void setStart(double x, double y) {
        this.startX = x;
        this.startY = y;
    }

    /**
     * Set the ending point of the line.
     */
    @Override
    public void setEnd(double x, double y) {
        this.endX = x;
        this.endY = y;
    }

    @Override
    public void draw(GraphicsContext gc) {
        gc.setStroke(strokeColor);
        gc.setLineWidth(strokeWidth);
        gc.strokeLine(startX, startY, endX, endY);
    }

    @Override
    public boolean contains(double x, double y) {
        // Check if the point is outside the bounding box of the line
        
        double minX = Math.min(startX, endX);
        double maxX = Math.max(startX, endX);
        double minY = Math.min(startY, endY);
        double maxY = Math.max(startY, endY);
        
        if (x < minX || x > maxX || y < minY || y > maxY) {
            return false;
        }

        // Calculate the distance from the point to the line segment
        double distance = Math.abs(
            (endY - startY) * x - (endX - startX) * y + endX * startY - endY * startX
        ) / Math.sqrt(Math.pow(endY - startY, 2) + Math.pow(endX - startX, 2));

        return distance <= strokeWidth / 2;
    }

    @Override
    public String getType() {
        return "Line";
    }
}