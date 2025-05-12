import javafx.scene.paint.Color;

// Triangle shape, can be either filled or just an outline,
// interprated as a polygon with 3 points
public class Triangle extends NPolygon {
    private static final int NUM_POINTS = 3;

    // Create a triangle with the given parameters,
    // the coordinates do not have to be in any specific order.
    public Triangle(
        double startX, double startY, double endX, double endY,
        Color fillColor, Color strokeColor, double strokeWidth
    ) {
        super(
            startX, startY, endX, endY, NUM_POINTS, 
            fillColor, strokeColor, strokeWidth, Math.PI / 6
        );
    }
    
    @Override
    public String getType() {
        return "Triangle";
    }
}
