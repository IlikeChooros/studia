import java.util.ArrayList;
import java.util.List;

import javafx.scene.canvas.*;
import javafx.scene.paint.Color;

interface IShapeType {
    SerializableShape getShape(
        double startX, double startY, double endX, double endY,
        Color fillColor, Color strokeColor, double strokeWidth
    );
}

public class DrawingBoard extends Canvas {

    /**
     * Enum for different shapes to draw.
     */
    public enum ShapeType implements IShapeType {
        LINE {
            public SerializableShape getShape(
                double startX, double startY, double endX, double endY,
                Color fillColor, Color strokeColor, double strokeWidth
            ) {
                return new Line(startX, startY, endX, endY,
                    strokeColor, strokeWidth);
            }
        },

        RECTANGLE {
            public SerializableShape getShape(
                double startX, double startY, double endX, double endY,
                Color fillColor, Color strokeColor, double strokeWidth
            ) {
                return new Rectangle(startX, startY, endX, endY, 
                    fillColor, strokeColor, strokeWidth, 0);
            }
        },

        CIRCLE {
            public SerializableShape getShape(
                double startX, double startY, double endX, double endY,
                Color fillColor, Color strokeColor, double strokeWidth
            ) {
                return new Circle(startX, startY, endX, endY,
                    fillColor, strokeColor, strokeWidth);
            }
        },
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

            SerializableShape shape = shapeType.getShape(
                startX, startY, endX, endY, fillColor, strokeColor, strokeWidth
            );
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
     * Clear the drawing board.
     */
    public void clear() {
        context.clearRect(0, 0, getWidth(), getHeight());
        context.setFill(Color.WHITE);
        context.fillRect(0, 0, getWidth(), getHeight());
    }
}

