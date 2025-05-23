import javafx.scene.paint.Color;

/**
 * A regular Pentagon
 */
public class Pentagon extends RegularPolygon {
    private static final long serialVersionUID = 50L;
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

    @Override
    public String getType() {
        return "Pentagon";
    }
}
