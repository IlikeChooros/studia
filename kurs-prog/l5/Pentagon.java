import javafx.scene.paint.Color;

public class Pentagon extends NPolygon {
    private static final int NUM_POINTS = 5;

    public Pentagon(
        double startX, double startY, double endX, double endY,
        Color fillColor, Color strokeColor, double strokeWidth
    ) {
        super(
            startX, startY, endX, endY, NUM_POINTS, 
            fillColor, strokeColor, strokeWidth, -Math.PI / 10
        );
    }
}
