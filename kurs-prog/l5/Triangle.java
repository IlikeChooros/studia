import javafx.scene.paint.Color;

public class Triangle extends NPolygon {
    private static final int NUM_POINTS = 3;

    public Triangle(
        double startX, double startY, double endX, double endY,
        Color fillColor, Color strokeColor, double strokeWidth
    ) {
        super(
            startX, startY, endX, endY, NUM_POINTS, 
            fillColor, strokeColor, strokeWidth, Math.PI / 6
        );
    }
    
}
