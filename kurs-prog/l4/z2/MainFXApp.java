import javafx.application.Application;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane; // Import ScrollPane
import javafx.scene.control.TextField; // Use TextField instead of TextArea
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.scene.text.Font;
// Removed unused imports like TextArea, EventHandler, ActionEvent (using lambda)

public class MainFXApp extends Application {

    // Declare rowBox and statusLabel as member variables to access in event handler
    private VBox rowBox;
    private TextField inputField; // Changed from TextArea

    public static void main(String[] args) {
        Application.launch(args);
    }

    @Override
    public void start(Stage stage) {
        // Top part
        Label inputLabel = new Label("Wprowadź liczbę wierszy:");
        inputField = new TextField();
        inputField.setOnAction(e -> handleGenerateAction());

        // Create input layout
        HBox inputBox = new HBox();
        inputBox.setSpacing(10);
        inputBox.setPadding(new Insets(10));
        inputBox.setAlignment(Pos.CENTER_LEFT);
        inputBox.getChildren().addAll(inputLabel, inputField);

        // Center part
        rowBox = new VBox();
        rowBox.setPadding(new Insets(10));
        rowBox.setSpacing(5);
        rowBox.setAlignment(Pos.TOP_CENTER);

        // Root layout
        BorderPane root = new BorderPane();
        root.setTop(inputBox);
        root.setCenter(rowBox);

        // Scene
        Scene scene = new Scene(root, 800, 600);
        stage.setTitle("Trojkat Pascala");
        stage.setScene(scene);
        stage.show();
    }

    // Separate method to handle the button/enter action
    private void handleGenerateAction() {
        String input = inputField.getText().trim(); // Get text from TextField
        try {
            int n = Integer.parseInt(input);
            TrojkatPascala trojkat = new TrojkatPascala(n);
            int[][] arr = trojkat.getArr();

            rowBox.getChildren().clear(); // Clear previous rows
            for (int i = 0; i < arr.length; i++) {
                String rowString = "";

                for (int j = 0; j <= i; j++) {
                    rowString += arr[i][j];
                    if (j < i) {
                        rowString += "  ";
                    }
                }

                Label rowLabel = new Label(rowString);
                rowLabel.setFont(new Font("monospaced", 14));
                rowLabel.setTextFill(Color.BLACK);
                rowLabel.setAlignment(Pos.CENTER);
                rowBox.getChildren().add(rowLabel);
            }
        } catch (NumberFormatException e) {
            System.out.println("Error: Invalid input");
            // statusLabel.setText("Error: Invalid input. Please enter an integer.");
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}