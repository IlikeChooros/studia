import java.net.*;
import java.io.*;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.stage.Stage;


public class Client extends Application {

    @Override
    public void start(Stage primaryStage) {

        ConsoleViewer viewer = new ConsoleViewer();
        Scene scene = new Scene(viewer);
        primaryStage.setScene(scene);
        primaryStage.setTitle("Binary Tree Viewer");
        primaryStage.show();
    }
}