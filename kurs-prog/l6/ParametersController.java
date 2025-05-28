// filepath: /home/who-ate-my-ram/studia/kurs-prog/l6/ParametersController.java
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

public class ParametersController {

    @FXML private Spinner<Integer> nRowsSpinner;
    @FXML private Spinner<Integer> nColsSpinner;
    @FXML private Spinner<Integer> wolfCountSpinner;
    @FXML private Spinner<Integer> rabbitCountSpinner;
    @FXML private TextField cycleRateField;
    @FXML private Spinner<Integer> wolfRangeSpinner;
    @FXML private Spinner<Integer> rabbitRangeSpinner;
    @FXML private Spinner<Integer> wolfSpeedSpinner;
    @FXML private Spinner<Integer> rabbitSpeedSpinner;
    @FXML private ComboBox<MovePolicy.Policies> wolfMovePolicyComboBox;
    @FXML private ComboBox<MovePolicy.Policies> rabbitMovePolicyComboBox;
    @FXML private Button startButton;

    private Stage stage;

    public void setStage(Stage stage) {
        this.stage = stage;
    }

    @FXML
    public void initialize() {
        // Initialize spinners with default values from SParameters and set ranges
        nRowsSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(5, 100, SParameters.nRows));
        nColsSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(5, 100, SParameters.nCols));
        wolfCountSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(0, 500, SParameters.wolfCount));
        rabbitCountSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(0, 500, SParameters.rabbitCount));
        cycleRateField.setText(String.valueOf(SParameters.cycleRate));
        wolfRangeSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 100, SParameters.wolfRange));
        rabbitRangeSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 100, SParameters.rabbitRange));
        wolfSpeedSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 10, SParameters.wolfSpeed));
        rabbitSpeedSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 10, SParameters.rabbitSpeed));
        
        // Initialize wolf and rabbit policy comboboxes with all available policies
        wolfMovePolicyComboBox.getItems().addAll(MovePolicy.Policies.values());
        rabbitMovePolicyComboBox.getItems().addAll(MovePolicy.Policies.values());
        
        // Set default values from SParameters
        wolfMovePolicyComboBox.setValue(SParameters.wolfMovePolicy);
        rabbitMovePolicyComboBox.setValue(SParameters.rabbitMovePolicy);
    }

    @FXML
    private void handleStartSimulation() {
        try {
            SParameters.nRows = nRowsSpinner.getValue();
            SParameters.nCols = nColsSpinner.getValue();
            SParameters.wolfCount = wolfCountSpinner.getValue();
            SParameters.rabbitCount = rabbitCountSpinner.getValue();
            SParameters.cycleRate = Double.parseDouble(cycleRateField.getText());
            SParameters.wolfRange = wolfRangeSpinner.getValue();
            SParameters.rabbitRange = rabbitRangeSpinner.getValue();
            SParameters.wolfSpeed = wolfSpeedSpinner.getValue();
            SParameters.rabbitSpeed = rabbitSpeedSpinner.getValue();
            SParameters.wolfMovePolicy = wolfMovePolicyComboBox.getValue();
            SParameters.rabbitMovePolicy = rabbitMovePolicyComboBox.getValue();

            // Ensure ranges are not greater than board dimensions if that's a constraint
            // For now, we assume they can be independent as per SParameters current structure

            if (SParameters.cycleRate <= 0) {
                showAlert("Invalid Input", "Cycle Rate must be a positive number.");
                return;
            }
            
            // Proceed to start the simulation
            // The Main class will handle creating the Manager and Board scene
            if (stage != null) {
                Main mainApp = new Main(); // Need an instance if startSimulationScene is not static
                mainApp.startSimulationScene(stage);
            }

        } catch (NumberFormatException e) {
            showAlert("Invalid Input", "Please enter valid numbers for all parameters.\nCycle Rate must be a number (e.g., 100.0).");
        } catch (Exception e) {
            showAlert("Error", "An unexpected error occurred: " + e.getMessage());
            e.printStackTrace();
        }
    }

    public static void showAlert(String title, String content) {
        Alert alert = new Alert(AlertType.ERROR);
        alert.setTitle(title);
        alert.setHeaderText(null);
        alert.setContentText(content);
        alert.showAndWait();
    }
}