// filepath: /home/who-ate-my-ram/studia/kurs-prog/l6/ParametersController.java
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.control.TextField;
import javafx.scene.text.Text;
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
    @FXML private ComboBox<String> wolfMovePolicyComboBox;
    @FXML private ComboBox<String> rabbitMovePolicyComboBox;
    @FXML private Text wolfPolicyText;
    @FXML private Text rabbitPolicyText;
    @FXML private Button startButton;

    private Stage stage;

    public void setStage(Stage stage) {
        this.stage = stage;
    }

    @FXML
    public void initialize() {
        // Initialize spinners with default values from SParameters and set ranges
        nRowsSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(5, 200, SParameters.nRows));
        nColsSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(5, 200, SParameters.nCols));
        wolfCountSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(0, 500, SParameters.wolfCount));
        rabbitCountSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(0, 5000, SParameters.rabbitCount));
        cycleRateField.setText(String.valueOf(SParameters.cycleRate));
        wolfRangeSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 100, SParameters.wolfRange));
        rabbitRangeSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 100, SParameters.rabbitRange));
        wolfSpeedSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 10, SParameters.wolfSpeed));
        rabbitSpeedSpinner.setValueFactory(new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 10, SParameters.rabbitSpeed));
        
        // Initialize wolf and rabbit policy comboboxes with all available policies
        wolfMovePolicyComboBox.getItems().addAll(
            MovePolicy.getPoliciesNames(
                null, 
                MovePolicy.Policies.RANDOM, 
                MovePolicy.Policies.ALWAYS_TOWRADS_RABBITS,
                MovePolicy.Policies.WAIT_AFTER_KILL
        ));

        rabbitMovePolicyComboBox.getItems().addAll(
            MovePolicy.getPoliciesNames(
                null,
                MovePolicy.Policies.RANDOM,
                MovePolicy.Policies.ALWAYS_RUN_AWAY_FROM_WOLVES,
                MovePolicy.Policies.RABBIT_PROBLEM_MOVEMENT
        ));
        
        // Set default values from SParameters
        wolfMovePolicyComboBox.setValue(MovePolicy.getPolicyNameEN(SParameters.wolfMovePolicy));
        rabbitMovePolicyComboBox.setValue(MovePolicy.getPolicyNameEN(SParameters.rabbitMovePolicy));

        // Set up policy descriptions
        setupPolicyDescriptions();
        
        // Set initial description text
        updateWolfPolicyDescription(wolfMovePolicyComboBox.getValue());
        updateRabbitPolicyDescription(rabbitMovePolicyComboBox.getValue());
    }

    /**
     * Set up change listeners for policy descriptions
     */
    private void setupPolicyDescriptions() {
        // Add listener to wolf policy combo box
        wolfMovePolicyComboBox.valueProperty().addListener((observable, oldValue, newValue) -> {
            updateWolfPolicyDescription(newValue);
        });
        
        // Add listener to rabbit policy combo box
        rabbitMovePolicyComboBox.valueProperty().addListener((observable, oldValue, newValue) -> {
            updateRabbitPolicyDescription(newValue);
        });
    }
    
    /**
     * Update wolf policy description based on selected policy
     */
    private void updateWolfPolicyDescription(String policyName) {
        if (policyName == null) return;
        
        String description = "";
        if (policyName.equals(MovePolicy.getPolicyNameEN(MovePolicy.Policies.RANDOM))) {
            description = "Wolves move randomly without targeting rabbits.";
        } else if (policyName.equals(MovePolicy.getPolicyNameEN(MovePolicy.Policies.ALWAYS_TOWRADS_RABBITS))) {
            description = "Wolves always move toward the closest rabbit within range.";
        } else if (policyName.equals(MovePolicy.getPolicyNameEN(MovePolicy.Policies.WAIT_AFTER_KILL))) {
            description = "Wolves chase rabbits and wait for 5 cycles after capturing one.";
        }

        wolfPolicyText.setText(description);
    }
    
    /**
     * Update rabbit policy description based on selected policy
     */
    private void updateRabbitPolicyDescription(String policyName) {
        if (policyName == null) return;
        
        String description = "";
        if (policyName.equals(MovePolicy.getPolicyNameEN(MovePolicy.Policies.RANDOM))) {
            description = "Rabbits move randomly without awareness of wolves.";
        } else if (policyName.equals(MovePolicy.getPolicyNameEN(MovePolicy.Policies.ALWAYS_RUN_AWAY_FROM_WOLVES))) {
            description = "Rabbits always try to move away from the closest wolf within range.";
        } else if (policyName.equals(MovePolicy.getPolicyNameEN(MovePolicy.Policies.RABBIT_PROBLEM_MOVEMENT))) {
            description = "Complex movement: rabbits flee from wolves, choose random directions at walls, and make uniform random choices when multiple escape paths exist.";
        }
        
        rabbitPolicyText.setText(description);
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


            // Set the movement policies
            String wolfPolicyString = wolfMovePolicyComboBox.getValue();
            String rabbitPolicyString = rabbitMovePolicyComboBox.getValue();

            SParameters.wolfMovePolicy = null;
            SParameters.rabbitMovePolicy = null;

            // Find the matching option
             MovePolicy.Policies policies[] = MovePolicy.Policies.values();
            for (int i = 0; (i < policies.length); i++) {
                if (wolfPolicyString == MovePolicy.getPolicyNameEN(policies[i])) {
                    SParameters.wolfMovePolicy = policies[i];
                }
                if (rabbitPolicyString == MovePolicy.getPolicyNameEN(policies[i])) {
                    SParameters.rabbitMovePolicy = policies[i];
                }
            }

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

    /**
     * Helper function to show alert
     */
    public static void showAlert(String title, String content) {
        Alert alert = new Alert(AlertType.ERROR);
        alert.setTitle(title);
        alert.setHeaderText(null);
        alert.setContentText(content);
        alert.showAndWait();
    }
}