import java.util.Optional;

import javafx.application.Application;
import javafx.scene.*;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

public class Main extends Application {
    
    private DrawingBoard drawingBoard;

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        // Create a simple scene with a button
        final double DEFAULT_WIDTH = 1200, DEFAULT_HEIGHT = 800;

        drawingBoard = new DrawingBoard(DEFAULT_WIDTH, DEFAULT_HEIGHT);
        BorderPane root = new BorderPane();
        PaintToolBar toolBar = new PaintToolBar(drawingBoard);
        StatusBar statusBar = new StatusBar(drawingBoard);

        toolBar.setPrefHeight(StatusBar.DEFAULT_HEIGHT);

        Scene scene = new Scene(root);
        root.setTop(toolBar);
        root.setCenter(drawingBoard);
        root.setBottom(statusBar);

        primaryStage.sizeToScene();
        primaryStage.setScene(scene);
        primaryStage.setTitle(PaintToolBar.WINDOW_BASE_NAME);
        primaryStage.show();

        // Make alert propmt, to urge the user to save the file
        primaryStage.setOnCloseRequest(event -> {
            // Check if drawing was modified
            if (drawingBoard.isModified()) {
                Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
                alert.setTitle("Unsaved Changes");
                alert.setHeaderText("You have unsaved changes.");
                alert.setContentText("Do you want to save before exiting?");
                
                ButtonType saveButton = new ButtonType("Save");
                ButtonType dontSaveButton = new ButtonType("Don't Save");
                ButtonType cancelButton = new ButtonType("Cancel", ButtonBar.ButtonData.CANCEL_CLOSE);
                alert.getButtonTypes().setAll(saveButton, dontSaveButton, cancelButton);
                
                Optional<ButtonType> result = alert.showAndWait();
                if (result.isPresent()) {
                    if (result.get() == saveButton) {
                        FileManager.save(drawingBoard, primaryStage);
                    } else if (result.get() == cancelButton) {
                        event.consume();
                    }
                }
            }
        });
    }

}
