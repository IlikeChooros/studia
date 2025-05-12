import javafx.scene.canvas.GraphicsContext;

// To modify the shapes on the canvas, we need to store the shapes as
// objects. These objects should implement a drawing method, selection,
// rotation, and allow for resizing (with scroll) and moving.

// Additionally, these objects should be serializable so that we can
// save the canvas as well as the shapes into a file

interface IShape {
    void draw(GraphicsContext gc);
    void setStart(double x, double y);
    void setEnd(double x, double y);
}

interface SerializableShape extends IShape {
    // This interface can be used to mark shapes that can be serialized
    // For example, we can add methods for serialization and deserialization
    // if needed in the future.
}





