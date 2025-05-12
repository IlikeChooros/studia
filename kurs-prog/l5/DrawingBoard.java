import java.util.Iterator;
import java.util.LinkedList;

import javafx.scene.canvas.*;
import javafx.scene.image.WritableImage;
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
    private LinkedList<BaseShape> shapes = new LinkedList<>();
    private WritableImage canvasSnapshot = null;

    /**
     * Get the shape to draw based on the current shape type.
     */
    private BaseShape getShape()
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

            // Creat a snapshot of the current canvas
            canvasSnapshot = this.snapshot(null, null);

            // Initialize end coordinates
            endX = startX;
            endY = startY;
        });

        setOnMouseDragged(e -> {
            endX = e.getX();
            endY = e.getY();

            // Draw the snapshot of the canvas
            if (this.canvasSnapshot != null) {
                context.drawImage(canvasSnapshot, 0, 0);

                // Draw the shape we are currently creating
                BaseShape shape = getShape();
                shape.draw(context);
            } 
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

            BaseShape shape = getShape();
            shape.draw(context);
            shapes.addFirst(shape);

            canvasSnapshot = null; // Clear the snapshot and redraw the canvas
            clear();
            drawShapes();
        });
    }

    /**
     * Since the `onMouseMoved` event is already handled by the
     * StatusBar, we need to add a public method to handle the
     * `onMouseMoved` event for the drawing board.
     * @param e the mouse event
     */
    public void onMouseMovedCallback(javafx.scene.input.MouseEvent e) {
        // Check if the mouse hovers over a shape
        for (BaseShape shape : shapes) {
            if (shape.contains(e.getX(), e.getY())) {
                setCursor(javafx.scene.Cursor.HAND);
                System.out.println("Mouse is over a shape: " + shape.getType());
                return;
            }
        }
        setCursor(javafx.scene.Cursor.DEFAULT);
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
            shapes.removeFirst();
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
        // Draw all shapes on the canvas starting from the bottom
        for (Iterator<BaseShape> it = shapes.descendingIterator(); it.hasNext();) {
            BaseShape shape = it.next();
            shape.draw(context);
        }
    }
}

