import javafx.scene.paint.Color;

public class Hexagon extends NPolygon {
    private static final int NUM_POINTS = 6;

    public Hexagon(
        double startX, double startY, double endX, double endY,
        Color fillColor, Color strokeColor, double strokeWidth
    ) {
        super(
            startX, startY, endX, endY, NUM_POINTS, 
            fillColor, strokeColor, strokeWidth, 0
        );
    }
}

