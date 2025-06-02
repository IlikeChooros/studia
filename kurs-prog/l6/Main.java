
import javafx.application.Application;
import javafx.scene.*;
import javafx.scene.control.Alert;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.application.Platform;
import javafx.fxml.FXMLLoader;
import javafx.scene.image.Image;

import java.io.IOException;
import java.net.URL;

public class Main extends Application {
    
    private static final String SIMULATION_WINDOW_TITLE = "Wolf Rabbit Simulation";
    private static final String PARAMETERS_WINDOW_TITLE = "Simulation Parameters";

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        try {
            FXMLLoader loader = new FXMLLoader();

            // Make sure the path to fxml is correct, assuming it's in 'resources' folder
            // and 'resources' is on the classpath or specified correctly.
            URL fxmlUrl = getClass().getResource("parameters.fxml"); 
            if (fxmlUrl == null) {
                System.err.println("Cannot find FXML file. Make sure 'resources' is in classpath.");
                return;
            }
            loader.setLocation(fxmlUrl);
            Parent root = loader.load();

            ParametersController controller = loader.getController();
            controller.setStage(primaryStage);

            Scene scene = new Scene(root);
            URL cssUrl = getClass().getResource("styles.css");
            if (cssUrl != null) {
                scene.getStylesheets().add(cssUrl.toExternalForm());
            } else {
                System.err.println("Cannot find CSS file. Styles will not be applied.");
            }
            
            primaryStage.setTitle(PARAMETERS_WINDOW_TITLE);
            // primaryStage.setResizable(false);
            primaryStage.getIcons().addAll(
                new Image("main-icon64.png"),
                new Image("main-icon32.png"),
                new Image("main-icon16.png")
            );
            primaryStage.setScene(scene);
            primaryStage.show();

        } catch (IOException e) {
            e.printStackTrace();
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Error Loading FXML");
            alert.setHeaderText("Could not load the parameters window.");
            alert.setContentText("Please check the console for details.");
            alert.showAndWait();
        }
    }

    public void startSimulationScene(Stage primaryStage) {
        Manager manager = new Manager();
        Scene simulationScene = new Scene(manager.getUIBoard());
        simulationScene.setFill(Color.BLACK);
        
        URL cssUrl = getClass().getResource("styles.css");
        if (cssUrl != null) {
            simulationScene.getStylesheets().add(cssUrl.toExternalForm());
        }

        primaryStage.sizeToScene();
        primaryStage.setScene(simulationScene);
        primaryStage.setTitle(SIMULATION_WINDOW_TITLE);

        primaryStage.setOnCloseRequest((event) -> {
            manager.kill(); // Ensure simulation threads are stopped
            Platform.exit();
        });
    }
}