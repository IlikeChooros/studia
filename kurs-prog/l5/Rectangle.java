import javafx.scene.paint.Color;

// Rectangle shape, can be either filled or just an outline
public class Rectangle extends Polygon {
    private static final long serialVersionUID = 60L;

    /**
     * Create a rectangle with the given parameters, 
     * the coordinates do not have to be in any specific order.
     * 
     * @param startX the starting x-coordinate
     * @param startY the starting y-coordinate 
     * @param endX ending x-coordinate
     * @param endY ending y-coordinate
     * @param fillColor null if no fill is needed
     * @param strokeColor the color of the outline
     * @param strokeWidth the width of the outline
     * @param rotation the rotation of the rectangle
     */
    public Rectangle(
        double startX, double startY, double endX, double endY,
        Color fillColor, Color strokeColor, double strokeWidth,
        double rotation
    ) {
        super(
            startX, startY, fillColor, strokeColor, strokeWidth
        );

        addPoint(endX, startY);
        addPoint(endX, endY);
        addPoint(startX, endY);
    }


    /**
     * Set the ending point of the rectangle.
     * This is used for creating the rectangle
     */
    @Override
    public void setEnd(double x, double y) {
        PolygonState state = getLastState();
        state.xPoints[1] = x;
        state.xPoints[2] = x;
        state.yPoints[2] = y;
        state.yPoints[3] = y;
    }

    /**
     * Get the type of the shape.
     * @return the type of the shape
     */
    @Override
    public String getType() {
        return "Rectangle";
    }
}