import javafx.application.Application;
import javafx.scene.*;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

public class Main extends Application {
    
    private static final String WINDOW_BASE_NAME = "h";

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        // Create a simple scene with a button
        // final double DEFAULT_WIDTH = 1200, DEFAULT_HEIGHT = 800;

        // BorderPane root = new BorderPane();
        Board board = new Board(20, 30, 20);
        Scene scene = new Scene(board);

        primaryStage.sizeToScene();
        primaryStage.setScene(scene);
        primaryStage.setTitle(WINDOW_BASE_NAME);
        primaryStage.show();
    }

}
