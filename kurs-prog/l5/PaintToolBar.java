import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.ColorPicker;
import javafx.scene.control.Separator;
import javafx.scene.control.ToolBar;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.scene.control.MenuButton;
import javafx.scene.control.MenuItem;
import javafx.scene.control.Label;

public class PaintToolBar extends ToolBar {
    private final DrawingBoard drawingBoard;
    static final String WINDOW_BASE_NAME = "PaintFX";

    /**
     * Create a toolbar for selecting shapes to draw.
     * @param db reference to the drawing board
     */
    public PaintToolBar(DrawingBoard db) {
        this.drawingBoard = db;
        setPadding(new Insets(5));

        // FILE menu (unchanged)
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

        // Create a ToggleGroup for shape selection.
        ToggleGroup shapeToggleGroup = new ToggleGroup();

        // Create ToggleButtons for all shapes.
        // For each shape, we use an icon. The "select" (NONE) button indicates selection mode.
        ToggleButton selectBtn = createShapeToggleButton("icons/select.png", "Select", DrawingBoard.ShapeType.NONE);
        ToggleButton triangleBtn = createShapeToggleButton("icons/triangle.png", "Triangle", DrawingBoard.ShapeType.TRIANGLE);
        ToggleButton rectangleBtn = createShapeToggleButton("icons/rectangle.png", "Rectangle", DrawingBoard.ShapeType.RECTANGLE);
        ToggleButton pentagonBtn = createShapeToggleButton("/icons/pentagon.png", "Pentagon", DrawingBoard.ShapeType.PENTAGON);
        ToggleButton hexagonBtn = createShapeToggleButton("/icons/hexagon.png", "Hexagon", DrawingBoard.ShapeType.HEXAGON);
        ToggleButton circleBtn = createShapeToggleButton("/icons/oval.png", "Circle", DrawingBoard.ShapeType.CIRCLE);
        ToggleButton polygonBtn = createShapeToggleButton("/icons/polygon.png", "Polygon", DrawingBoard.ShapeType.POLYGON);

        // Add all shape buttons to the group
        shapeToggleGroup.getToggles().addAll(
            selectBtn, triangleBtn, rectangleBtn,
            pentagonBtn, hexagonBtn, circleBtn, polygonBtn
        );

        // Set selectBtn as the default active (which implies shapeType.NONE)
        selectBtn.setSelected(true);
        drawingBoard.setShapeType(DrawingBoard.ShapeType.NONE);

        // When a toggle is selected, update the drawing board's shape type.
        shapeToggleGroup.selectedToggleProperty().addListener((obs, oldVal, newVal) -> {
            if (newVal != null) {
                DrawingBoard.ShapeType newType = (DrawingBoard.ShapeType) newVal.getUserData();
                drawingBoard.setShapeType(newType);
            }
        });

        // Group the shape buttons in a horizontal box for neat organization.
        HBox shapeButtonsBox = new HBox(5, selectBtn, triangleBtn, rectangleBtn 
            ,pentagonBtn, hexagonBtn, circleBtn, polygonBtn
        );
        shapeButtonsBox.setPadding(new Insets(0, 10, 0, 10));

        // Create a color picker for stroke color
        ColorPicker strokeColorPicker = new ColorPicker();
        strokeColorPicker.setValue(Color.BLACK); // Default color
        strokeColorPicker.setTooltip(new Tooltip("Stroke Color"));
        strokeColorPicker.setOnAction(e -> {
            drawingBoard.setStrokeColor(strokeColorPicker.getValue());
        });

        // Create a color picker for fill color
        ColorPicker fillColorPicker = new ColorPicker();
        fillColorPicker.setValue(Color.WHITE); // Default color
        fillColorPicker.setTooltip(new Tooltip("Fill Color"));
        fillColorPicker.setOnAction(e -> {
            drawingBoard.setFillColor(fillColorPicker.getValue());
        });

        // Clear and Undo buttons
        Button clearButton = new Button("Clear");
        clearButton.setOnAction(e -> drawingBoard.clearShapes());
        Button undoButton = new Button("Undo");
        undoButton.setOnAction(e -> drawingBoard.undo());

        // Assemble the toolbar
        getItems().addAll(
            fileMenuButton,
            new Separator(),
            new Label("Shapes:"),
            shapeButtonsBox,
            new Separator(),
            new Label("Stroke:"),
            strokeColorPicker,
            new Separator(),
            new Label("Fill:"),
            fillColorPicker,
            new Separator(),
            clearButton,
            undoButton
        );
    }

    /**
     * Helper method to create a ToggleButton for shape selection.
     * @param iconPath the path to the icon image
     * @param tooltipText tooltip text for the button
     * @param type the associated DrawingBoard.ShapeType
     * @return a configured ToggleButton instance.
     */
    private ToggleButton createShapeToggleButton(String iconPath, String tooltipText, DrawingBoard.ShapeType type) {
        ToggleButton btn = new ToggleButton();
        ImageView iconView = new ImageView(new Image(getClass().getResourceAsStream(iconPath)));
        iconView.setFitWidth(24);
        iconView.setFitHeight(24);
        btn.setGraphic(iconView);
        btn.setTooltip(new Tooltip(tooltipText));
        btn.setUserData(type);
        return btn;
    }
}