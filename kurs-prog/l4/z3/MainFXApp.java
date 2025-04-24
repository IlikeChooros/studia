import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.List;
import java.util.ArrayList;

import javafx.application.Application;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.TextField; // Use TextField instead of TextArea
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.scene.text.Font;

public class MainFXApp extends Application {
    
    private Label commandResultLabel;
    private TextField inputField;

    public static void main(String[] args) {
        Application.launch(args);
    }

    @Override
    public void start(Stage stage) {

        // Top part
        Label inputLabel = new Label("Wprowadź argumenty:");
        inputField = new TextField();
        inputField.setOnAction(e -> handleAction());

        HBox inputBox = new HBox(5);
        inputBox.setPadding(new Insets(10));
        inputBox.setAlignment(Pos.CENTER_LEFT);
        inputBox.getChildren().addAll(inputLabel, inputField);

        // Center part
        VBox resultBox = new VBox();
        resultBox.setPadding(new Insets(10));
        resultBox.setSpacing(5);
        resultBox.setAlignment(Pos.TOP_CENTER);

        Label resultLabel = new Label("Wynik:");
        resultLabel.setFont(new Font(16));
        resultLabel.setTextFill(Color.DARKGRAY);
        resultLabel.setAlignment(Pos.TOP_LEFT);

        commandResultLabel = new Label();
        commandResultLabel.setFont(new Font(16));
        commandResultLabel.setTextFill(Color.BLACK);
        commandResultLabel.setAlignment(Pos.TOP_LEFT);
        
        resultBox.getChildren().addAll(resultLabel, commandResultLabel);

        // Root layout
        BorderPane root = new BorderPane();
        root.setTop(inputBox);
        root.setCenter(resultBox);
        root.setPadding(new Insets(10));

        Scene scene = new Scene(root, 400, 600);
        stage.setTitle("Trojkat Pascala");
        stage.setScene(scene);
        stage.show();
    }

    private void handleAction() {
        
        // Create command list
        String input = inputField.getText().trim();
        List<String> commands = new ArrayList<>();
        commands.add("./main.out");

        String[] args = input.split(" ");
        for (String arg : args) {
            commands.add(arg);
        }

        String result = new String();
        ProcessBuilder processBuilder = new ProcessBuilder(commands);
        
        try {
            Process process = processBuilder.start();
            InputStreamReader reader = new InputStreamReader(process.getInputStream());
            BufferedReader bufferedReader = new BufferedReader(reader);

            // Read the output from the command
            String line = bufferedReader.readLine();
            while (line != null) {
                result += line + "\n";
                line = bufferedReader.readLine();
            }
            bufferedReader.close();
            
            // Set the result to the label and wait for the process to finish
            commandResultLabel.setText(result);
            process.waitFor();

        } catch (Exception e) {
            commandResultLabel.setText("Wystąpił nieoczekiwany błąd: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
