import javafx.application.Application;
import javafx.scene.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.stage.Stage;


public class Main extends Application {
    
    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        // Create a simple scene with a button
        Group root = new Group();
        Text text = new Text("Hello, JavaFX!");
        text.setFont(Font.font("Arial", 32));
        text.setStyle("-fx-fill: white; -fx-center-text: true;");
        text.setX(50);
        text.setY(100);


        root.getChildren().add(text);

        Scene scene = new Scene(root, 400, 400, Color.web("#242424"));
        primaryStage.setScene(scene);
        primaryStage.show();
    }

}
