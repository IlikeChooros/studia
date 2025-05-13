import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ArrayList;

import javafx.scene.canvas.*;
import javafx.scene.image.WritableImage;
import javafx.scene.paint.Color;

public class DrawingBoard extends Canvas {

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
    private LinkedList<BShape> shapes = new LinkedList<>();
    private List<Integer> history = new ArrayList<>();
    private WritableImage canvasSnapshot = null;
    private BShape selectedShape = null;
    private int historyIdCounter = 0;
    private boolean validMove = false;
    private boolean validScroll = false;

    /**
     * Get the shape to draw based on the current shape type.
     */
    private BShape getShape()
    {
        BShape s = null;
        switch (shapeType) {
            case LINE:
                s = new Line(startX, startY, endX, endY, strokeColor, strokeWidth);
                break;
            case RECTANGLE:
                s = new Rectangle(startX, startY, endX, endY, fillColor, strokeColor, strokeWidth, 0);
                break;
            case CIRCLE:
                s = new Circle(startX, startY, endX, endY, fillColor, strokeColor, strokeWidth);
                break;
            case PENTAGON:
                s = new Pentagon(startX, startY, endX, endY, fillColor, strokeColor, strokeWidth);
                break;
            case HEXAGON:
                s = new Hexagon(startX, startY, endX, endY, fillColor, strokeColor, strokeWidth);
                break;
            case TRIANGLE:
                s = new Triangle(startX, startY, endX, endY, fillColor, strokeColor, strokeWidth);
                break;
            default:
                break;
        }

        if (s != null) {
            s.id = historyIdCounter++;
        }
        return s;
    }

    /**
     * Check if the mouse is over a shape.
     * @param x the x coordinate of the mouse
     * @param y the y coordinate of the mouse
     */
    private BShape getShapeOnCoords(double x, double y) {
        for (BShape shape : shapes) {
            if (shape.contains(x, y)) {
                return shape;
            }
        }
        return null;
    }

    /**
     * Enum for different shapes to draw.
     */
    public enum ShapeType {
        NONE, LINE, RECTANGLE, CIRCLE, PENTAGON, HEXAGON, TRIANGLE
    }

    public static final Color DEFAULT_BG_COLOR = Color.WHITESMOKE;

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
            // reset flags for history
            validMove = false;
            validScroll = false;

            startX = e.getX();
            startY = e.getY();

            // Initialize end coordinates
            endX = startX;
            endY = startY;

            // DONE: implement moving of the shape
            // 1. Check if the mouse is over a shape
            // 2. If no shape is selected (shapeType == ShapeType.NONE && selectedShape == null)
            // 3. Add easy to use move handles to 'BaseShape' (calculate the deltaX and deltaY and apply it to the shape)
            // 4. For live preview, make the snapshot without the selected shape, and similarly to creation, draw the 
            //   snapshot and the shape on top of it
            // 5. On mouse release, set the shape to the new coordinates
            // 6. Clear the canvas and redraw all shapes
            // 7. If the shape is selected, but outside of it's bounds, then set the selected shape to null
            // 7a. Make the cursor change (if drawing a pencil or smth, 
            // otherwise an arrow, if possible selction of figure -> pointer)

            // Step 1: Select the shape
            if (shapeType == ShapeType.NONE) {
                selectedShape = getShapeOnCoords(startX, startY);

                // We selected an actual shape
                if (selectedShape != null) {
                    // Find the selected shape and
                    // remove it temporarily (for snapshot)
                    shapes.remove(shapes.indexOf(selectedShape));
                    clear();
                    drawShapes();
                    // Create a snapshot of the current canvas & restore the shape
                    canvasSnapshot = this.snapshot(null, null);
                    shapes.addFirst(selectedShape);
                    drawShapes();
                    return;
                }
            }
    
            // Create a snapshot of the current canvas
            canvasSnapshot = this.snapshot(null, null);
        });

        setOnScroll(e -> {
            // If the mouse is over a shape, and we selected it
            // then we can resize it

            // TODO: implement resizing of the shape
            // 1. Add easy-to-use resize handles to 'BaseShape'
            // 2. Make sure NONE is selected (as a shape) when resizing (shapeType == ShapeType.NONE)
            // 3. No need to create a snapshot of the canvas, it's already created
            // 4. Apply deltaX and deltaY to the shape

            if (selectedShape == null || shapeType != ShapeType.NONE)
                return;
            

            // Push history
            if (!validScroll) {
                selectedShape.copyState(); // push new state
                validScroll = true;
                history.add(selectedShape.id);
            }

            System.out.println("Scroll: " + e.getDeltaX() + " " + e.getDeltaY());
            selectedShape.resize(e.getDeltaY() / 5);

            // Draw the resized shape
            context.drawImage(canvasSnapshot, 0, 0);
            selectedShape.draw(context);
        });

        setOnMouseDragged(e -> {
            double dx = e.getX() - endX;
            double dy = e.getY() - endY;

            endX = e.getX();
            endY = e.getY();

            // DONE: Apply move vector
            if (shapeType == ShapeType.NONE && selectedShape != null) {
                // Push history
                if (!validMove) {
                    selectedShape.copyState(); // push new state
                    validMove = true;
                    history.add(selectedShape.id);
                }
                selectedShape.move(dx, dy);
            }

            // Draw the snapshot of the canvas
            if (this.canvasSnapshot != null) {
                context.drawImage(canvasSnapshot, 0, 0);

                // Draw the shape we are currently creating
                if (shapeType != ShapeType.NONE) {
                    BShape shape = getShape();
                    shape.draw(context);
                }
                else {
                    // Draw the selected shape
                    if (selectedShape != null) {
                        selectedShape.draw(context);
                    }
                }
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

            if (shapeType != ShapeType.NONE) {
                // Draw new shape on canvas, and add it to the list
                BShape shape = getShape();
                shape.draw(context);
                shapes.addFirst(shape);
                history.add(shape.id); // push history

                canvasSnapshot = null; // Clear the snapshot and redraw the canvas
                clear();
                drawShapes();
            }
        });
    }

    /**
     * Since the `onMouseMoved` event is already handled by the
     * StatusBar, we need to add a public method to handle the
     * `onMouseMoved` event for the drawing board.
     * @param e the mouse event
     */
    public void onMouseMovedCallback(javafx.scene.input.MouseEvent e) {
        BShape shape = getShapeOnCoords(e.getX(), e.getY());

        // If there is nothing selected, only then indicate that the 
        // shape is selectable
        if (shape != null && shapeType == ShapeType.NONE) {
            setCursor(javafx.scene.Cursor.HAND);
        } 
        else {
            setCursor(javafx.scene.Cursor.DEFAULT);
        }
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
        if (history.isEmpty())
            return;
        
        clear();

        // Get last modified shape (or created) from history list
        int lastId = history.remove(history.size() - 1);
        int shapeIdx = 0;
        BShape shape = null;

        // Find the shape with given id
        for (BShape s : shapes) {
            if (s.id == lastId) {
                shape = s;
                break;
            }
            shapeIdx++;
        }

        // If the first shape on the list has history of modifications
        // restore the previous states
        if (shape.hasModifiedStates()) {
            shape.restoreState();
        }
        // Else remove the shape entirely
        else {
            shapes.remove(shapeIdx);
        }

        drawShapes();
    }

    /**
     * Clear the canvas and reset the background color.
     */
    public void clear() {
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
        for (Iterator<BShape> it = shapes.descendingIterator(); it.hasNext();) {
            BShape shape = it.next();
            shape.draw(context);
        }
    }

    // Getters

    /**
     * Gets the current list of shapes on the drawing board.
     */
    public LinkedList<BShape> getShapes() {
        return this.shapes;
    }

    /**
     * Gets the current history list.
     */
    public List<Integer> getHistory() {
        return this.history;
    }

    /**
     * Gets the current history ID counter.
     */
    public int getHistoryIdCounter() {
        return this.historyIdCounter;
    }

    /**
     * Loads drawing data (shapes, history, historyIdCounter) onto the drawing board,
     * replacing existing content.
     * @param loadedShapes The list of shapes to load.
     * @param loadedHistory The history list to load.
     * @param loadedHistoryIdCounter The history ID counter to load.
     */
    public void loadDrawingData(
        LinkedList<BShape> loadedShapes, 
        List<Integer> loadedHistory, 
        int loadedHistoryIdCounter
    ) {
        this.shapes = new LinkedList<>(loadedShapes); // Use a copy
        this.history = new ArrayList<>(loadedHistory); // Use a copy
        this.historyIdCounter = loadedHistoryIdCounter;
        
        this.selectedShape = null; // Reset selection
        this.canvasSnapshot = null; // Reset snapshot

        clear(); // Clear canvas (fills with background)
        drawShapes(); // Redraw all loaded shapes
        System.out.println("Drawing data loaded. Shapes: " + this.shapes.size() + ", History entries: " + this.history.size() + ", HistoryCounter: " + this.historyIdCounter);
    }
}

