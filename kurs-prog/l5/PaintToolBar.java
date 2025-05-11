
import javafx.scene.control.*;
import javafx.scene.paint.Color;

public class PaintToolBar extends ToolBar {
    private final DrawingBoard drawingBoard;


    /**
     * Create a toolbar for selecting shapes to draw.
     * @param db refrerence to the drawing board
     */
    public PaintToolBar(DrawingBoard db) {
        this.drawingBoard = db;

        // Create buttons for different shapes
        ComboBox<String> shapeComboBox = new ComboBox<>();
        shapeComboBox.getItems().addAll("Line", "Rectangle", "Circle", "Fill Rectangle", "Fill Circle");
        shapeComboBox.setValue("Line"); // Default selection

        shapeComboBox.setOnAction(e -> {
            String selectedShape = shapeComboBox.getValue();
            switch (selectedShape) {
                case "Line":
                    drawingBoard.setShapeType(DrawingBoard.ShapeType.LINE);
                    break;
                case "Rectangle":
                    drawingBoard.setShapeType(DrawingBoard.ShapeType.RECTANGLE);
                    break;
                case "Circle":
                    drawingBoard.setShapeType(DrawingBoard.ShapeType.CIRCLE);
                    break;
                case "Fill Rectangle":
                    drawingBoard.setShapeType(DrawingBoard.ShapeType.FILL_RECTANGLE);
                    break;
                case "Fill Circle":
                    drawingBoard.setShapeType(DrawingBoard.ShapeType.FILL_CIRCLE);
                    break;
                default:
                    drawingBoard.setShapeType(DrawingBoard.ShapeType.LINE);
                    break;
            }
        });

        // Create a color picker for stroke color
        ColorPicker strokeColorButton = new ColorPicker();
        strokeColorButton.setValue(Color.BLACK); // Default color
        strokeColorButton.setOnAction(e -> {
            drawingBoard.setStrokeColor(strokeColorButton.getValue());
        });

        // Create a color picker for fill color
        ColorPicker fillColorButton = new ColorPicker();
        fillColorButton.setValue(Color.WHITE); // Default color
        fillColorButton.setOnAction(e -> {
            drawingBoard.setFillColor(fillColorButton.getValue());
        });


        // Add buttons to the toolbar
        getItems().addAll(
            new Label("Shape:"),
            shapeComboBox,
            new Separator(), // Add a separator for better UI
            new Label("Stroke Color:"),
            strokeColorButton,
            new Separator(), // Add a separator for better UI
            new Label("Fill Color:"),
            fillColorButton
        );
    }
}
