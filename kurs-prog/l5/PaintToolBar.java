import javafx.scene.control.*;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

public class PaintToolBar extends ToolBar {
    private final DrawingBoard drawingBoard;

    static final String WINDOW_BASE_NAME = "PaintFX";


    /**
     * Create a toolbar for selecting shapes to draw.
     * @param db refrerence to the drawing board
     */
    public PaintToolBar(DrawingBoard db) {
        this.drawingBoard = db;

        // Add buttons to the toolbar
        MenuButton shapeMenu = new MenuButton("Select shape");

        DrawingBoard.ShapeType types[] = {
            DrawingBoard.ShapeType.NONE,
            DrawingBoard.ShapeType.LINE,
            DrawingBoard.ShapeType.TRIANGLE,
            DrawingBoard.ShapeType.RECTANGLE,
            DrawingBoard.ShapeType.PENTAGON,
            DrawingBoard.ShapeType.HEXAGON,
            DrawingBoard.ShapeType.CIRCLE
        };

        for (DrawingBoard.ShapeType type : types) {
            MenuItem m = new MenuItem(type.name().toLowerCase());
            m.setOnAction(e -> {
                shapeMenu.setText(m.getText());
                drawingBoard.setShapeType(type);
            });
            shapeMenu.getItems().add(m);
        }

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

        Button clearButton = new Button("Clear");
        clearButton.setOnAction(e -> {
            drawingBoard.clearShapes();
        });

        Button undoButton = new Button("Undo");
        undoButton.setOnAction(e -> {
            drawingBoard.undo();
        });

        // File operations using a MenuButton
        MenuButton fileMenuButton = new MenuButton("File");

        MenuItem saveMenuItem = new MenuItem("Save");
        saveMenuItem.setOnAction(e -> {
            Stage stage = (Stage) getScene().getWindow();
            String filename = FileManager.save(drawingBoard, stage);
            stage.setTitle(WINDOW_BASE_NAME + " " + filename);
        });

        MenuItem loadMenuItem = new MenuItem("Load");
        loadMenuItem.setOnAction(e -> {
            Stage stage = (Stage) getScene().getWindow();
            FileManager.DrawingData data = FileManager.load(stage);
            if (data != null) {
                stage.setTitle(WINDOW_BASE_NAME + " " + data.getFilename());
                drawingBoard.loadDrawingData(data.getShapes(), data.getHistory(), data.getHistoryIdCounter());
            }
        });

        fileMenuButton.getItems().addAll(saveMenuItem, loadMenuItem);


        // Add controls to the toolbar
        getItems().addAll(
            fileMenuButton, // Add the MenuButton for file operations
            shapeMenu,
            new Separator(), // Add a separator for better UI
            new Label("Stroke Color:"),
            strokeColorButton,
            new Separator(), // Add a separator for better UI
            new Label("Fill Color:"),
            fillColorButton,
            new Separator(), // Add a separator for better UI
            clearButton,
            undoButton
        );
    }
}
