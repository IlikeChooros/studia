import javafx.scene.canvas.*;
import javafx.scene.paint.Color;

interface IShape {
    void draw(
        GraphicsContext gc, double startX, 
        double startY, double endX, double endY
    );
}

public class DrawingBoard extends Canvas {

    /**
     * Enum for different shapes to draw.
     */
    public enum ShapeType implements IShape {
        LINE {
            @Override
            public void draw(
                GraphicsContext gc, double startX, 
                double startY, double endX, double endY
            ) {
                gc.strokeLine(startX, startY, endX, endY);
            }
        },

        RECTANGLE {
            @Override
            public void draw(
                GraphicsContext gc, double startX, 
                double startY, double endX, double endY
            ) {
                // Ensure the rectangle is drawn from the top-left corner
                // to the bottom-right corner
                double x0 = Math.min(startX, endX);
                double y0 = Math.min(startY, endY);
                double x1 = Math.max(startX, endX);
                double y1 = Math.max(startY, endY);

                // Draw the rectangle                
                gc.strokeRect(x0, y0, x1 - x0, y1 - y0);
            }
        },

        CIRCLE {
            @Override
            public void draw(
                GraphicsContext gc, double startX, 
                double startY, double endX, double endY
            ) {
                double radius = Math.sqrt(
                    Math.pow(endX - startX, 2) + 
                    Math.pow(endY - startY, 2)
                );                

                gc.strokeOval(startX - radius, startY - radius, 
                    radius * 2, radius * 2);
            }
        },

        FILL_RECTANGLE {
            @Override
            public void draw(
                GraphicsContext gc, double startX, 
                double startY, double endX, double endY
            ) {
                // Ensure the rectangle is drawn from the top-left corner
                // to the bottom-right corner
                double x0 = Math.min(startX, endX);
                double y0 = Math.min(startY, endY);
                double x1 = Math.max(startX, endX);
                double y1 = Math.max(startY, endY);

                // Draw the filled rectangle
                gc.fillRect(x0, y0, x1 - x0, y1 - y0);
                gc.strokeRect(x0, y0, x1 - x0, y1 - y0);
            }
        },

        FILL_CIRCLE {
            @Override
            public void draw(
                GraphicsContext gc, double startX, 
                double startY, double endX, double endY
            ) {
                double radius = Math.sqrt(
                    Math.pow(endX - startX, 2) + 
                    Math.pow(endY - startY, 2)
                );

                // Draw the filled circle
                gc.fillOval(startX - radius, startY - radius, 
                    radius * 2, radius * 2);
                gc.strokeOval(startX - radius, startY - radius, 
                    radius * 2, radius * 2);
            }
        };
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
            this.shapeType.draw(context, startX, startY, endX, endY);
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

