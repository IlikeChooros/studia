import java.util.ArrayList;
import java.util.List;

import javafx.scene.canvas.*;
import javafx.scene.paint.Color;

public class DrawingBoard extends Canvas {

    /**
     * Enum for different shapes to draw.
     */
    public enum ShapeType {
        LINE, RECTANGLE, CIRCLE, PENTAGON, HEXAGON, TRIANGLE
    }

    public static final Color DEFAULT_BG_COLOR = Color.WHITESMOKE;

    /**
     * Drawing context for this canvas.
     */
    private final GraphicsContext context;

    /**
     * Coordinates for the start and end points of the shape.
     */
    private double startX, startY;
    private double endX, endY;
    private ShapeType shapeType = ShapeType.LINE;
    private Color fillColor = Color.WHITE;
    private Color strokeColor = Color.BLACK;
    private double strokeWidth = 2;
    private List<SerializableShape> shapes = new ArrayList<>();

    /**
     * Get the shape to draw based on the current shape type.
     */
    private SerializableShape getShape()
    {
        switch (shapeType) {
            case LINE:
                return new Line(startX, startY, endX, endY, strokeColor, strokeWidth);
            case RECTANGLE:
                return new Rectangle(startX, startY, endX, endY, fillColor, strokeColor, strokeWidth, 0);
            case CIRCLE:
                return new Circle(startX, startY, endX, endY, fillColor, strokeColor, strokeWidth);
            case PENTAGON:
                return new Pentagon(startX, startY, endX, endY, fillColor, strokeColor, strokeWidth);
            case HEXAGON:
                return new Hexagon(startX, startY, endX, endY, fillColor, strokeColor, strokeWidth);
            case TRIANGLE:
                return new Triangle(startX, startY, endX, endY, fillColor, strokeColor, strokeWidth);
            
            default:
                return null;
        }
    }

    /**
     * Create a new drawing plane with the 
     * specified width and height.
     */
    public DrawingBoard(double width, double height) {
        super(width, height);
        context = getGraphicsContext2D();
        context.setFill(fillColor);
        context.setStroke(strokeColor);
        context.setLineWidth(strokeWidth);

        context.setFill(DEFAULT_BG_COLOR);
        context.fillRect(0, 0, width, height);

        setOnMousePressed(e -> {
            startX = e.getX();
            startY = e.getY();
        });

        setOnMouseDragged(e -> {
            endX = e.getX();
            endY = e.getY();
        });

        setShapeType(ShapeType.LINE);
    }

    /**
     * Set the shape type to draw.
     */
    public void setShapeType(ShapeType shapeType) {
        this.shapeType = shapeType;

        setOnMouseReleased(e -> {
            endX = e.getX();
            endY = e.getY();

            SerializableShape shape = getShape();
            shape.draw(context);
            shapes.add(shape);
        });
    }

    /**
     * Set the fill color for the shape.
     */
    public void setFillColor(Color color) {
        this.fillColor = color;
        context.setFill(color);
    }

    /**
     * Set the stroke color for the shape.
     */
    public void setStrokeColor(Color color) {
        this.strokeColor = color;
        context.setStroke(color);
    }

    /**
     * Set the stroke width for the shape.
     */
    public void setStrokeWidth(double width) {
        this.strokeWidth = width;
        context.setLineWidth(width);
    }

    /**
     * Get the current shape type.
     */
    public void undo() {
        if (!shapes.isEmpty()) {
            shapes.remove(shapes.size() - 1);
            clear();
            drawShapes();
        }
    }

    /**
     * Clear the canvas and reset the background color.
     */
    public void clear() {
        context.clearRect(0, 0, getWidth(), getHeight());
        context.setFill(DEFAULT_BG_COLOR);
        context.fillRect(0, 0, getWidth(), getHeight());
    }

    /**
     * Earase all shapes from the canvas, and redraw the background.
     */
    public void clearShapes() {
        shapes.clear();
        clear();
    }

    /**
     * Draw all shapes on the canvas.
     */
    public void drawShapes() {
        for (SerializableShape shape : shapes) {
            shape.draw(context);
        }
    }
}

