
import javafx.application.Application;
import javafx.scene.*;
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
        final double DEFAULT_WIDTH = 800, DEFAULT_HEIGHT = 600;

        drawingBoard = new DrawingBoard(DEFAULT_WIDTH, DEFAULT_HEIGHT);
        BorderPane root = new BorderPane();
        PaintToolBar toolBar = new PaintToolBar(drawingBoard);
        StatusBar statusBar = new StatusBar(drawingBoard);

        toolBar.setPrefHeight(StatusBar.DEFAULT_HEIGHT);

        root.setTop(toolBar);
        root.setCenter(drawingBoard);
        root.setBottom(statusBar);

        Scene scene = new Scene(
            root, DEFAULT_WIDTH, 
            drawingBoard.getHeight() + 2*StatusBar.DEFAULT_HEIGHT
        );
        primaryStage.setScene(scene);
        primaryStage.setTitle(PaintToolBar.WINDOW_BASE_NAME);
        primaryStage.show();
    }

}
