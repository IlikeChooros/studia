import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;

public class StatusBar extends HBox {

    public static final double DEFAULT_HEIGHT = 30.0;

    /**
     * Creates a simple status bar with a light gray background.
     * It displays the current mouse position when the mouse is moved
     * over the drawing area.
     */
    public StatusBar(DrawingBoard drawingBoard) {
        setStyle("-fx-background-color: lightgray; -fx-padding: 5; -fx-spacing: 10;");
        setPrefHeight(DEFAULT_HEIGHT);

        drawingBoard.setOnMouseMoved(event -> {
            double x = event.getX();
            double y = event.getY();
            this.updateStatus(String.format("Mouse Position: (%.2f, %.2f)", x, y));
            drawingBoard.onMouseMovedCallback(event);
        });
    }

    /**
     * Updates the status bar with the given status message.
     *
     * @param status The status message to display.
     */
    public void updateStatus(String status) {
        getChildren().clear();
        Label statusLabel = new Label(status);
        statusLabel.setTextFill(Color.BLACK);
        statusLabel.setFont(Font.font("Arial", 14));
        getChildren().add(new javafx.scene.control.Label(status));
    }

}
