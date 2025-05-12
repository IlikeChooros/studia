import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

public class Line implements SerializableShape {
    private double startX, startY, endX, endY;
    private Color strokeColor;
    private double strokeWidth;

    public Line(
        double startX, double startY, double endX, double endY,
        Color strokeColor, double strokeWidth
    ) {
        this.startX = startX;
        this.startY = startY;
        this.endX = endX;
        this.endY = endY;
        this.strokeColor = strokeColor;
        this.strokeWidth = strokeWidth;
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
}