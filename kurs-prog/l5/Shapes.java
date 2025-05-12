import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

// To modify the shapes on the canvas, we need to store the shapes as
// objects. These objects should implement a drawing method, selection,
// rotation, and allow for resizing (with scroll) and moving.

// Additionally, these objects should be serializable so that we can
// save the canvas as well as the shapes into a file

interface IShape {
    void draw(GraphicsContext gc);
    void setStart(double x, double y);
    void setEnd(double x, double y);
    boolean contains(double x, double y);
    String getType(); 
}

interface SerializableShape extends IShape {
    // This interface can be used to mark shapes that can be serialized
    // For example, we can add methods for serialization and deserialization
    // if needed in the future.
}

// Base class for shapes, can be extended for specific shapes
abstract class BaseShape implements SerializableShape {
    protected double startX, startY, endX, endY;
    protected Color fillColor;
    protected Color strokeColor;
    protected double strokeWidth;
    protected double rotation;

    public BaseShape(
        double startX, double startY, double endX, double endY,
        Color fillColor, Color strokeColor, double strokeWidth,
        double rotation
    ) {
        this.startX = startX;
        this.startY = startY;
        this.endX = endX;
        this.endY = endY;
        this.fillColor = fillColor;
        this.strokeColor = strokeColor;
        this.strokeWidth = strokeWidth;
        this.rotation = rotation;
    }
}




