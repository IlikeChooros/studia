import java.awt.*;
import java.awt.event.*;

public class Trojkat extends Frame implements ActionListener {
    
    private Panel rowPanel;
    private TextField inputField;

    public Trojkat() {
        super("Trojkat Pascala");
        setSize(800, 600);
        setLayout(new BorderLayout());

        // Input field at the top
        inputField = new TextField(10);
        inputField.addActionListener(this);
        add(inputField, BorderLayout.NORTH);

        // Panel for the rows in the center
        rowPanel = new Panel();
        rowPanel.setLayout(new GridLayout(0, 1));
        add(rowPanel, BorderLayout.CENTER);

        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent we) {
                System.exit(0);
            }
        });

        setVisible(true);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        // Check if the event source is our input field
        if (e.getSource() == inputField) {
            String input = inputField.getText();
            try {
                // Parse the input to an integer
                int n = Integer.parseInt(input);
                if (n < 0) {
                    showDialog("Liczba musi być nieujemna");
                    return;
                }

                // Delete previous rows
                rowPanel.removeAll();

                // Generate and add new rows
                TrojkatPascala trojkat = new TrojkatPascala(n);
                int[][] arr = trojkat.getArr();

                for (int i = 0; i < arr.length; i++) {
                    String napis = "";
                    for (int j = 0; j < arr[i].length; j++) {
                        if (arr[i][j] != 0) {
                            napis += arr[i][j];
                            if (j < i) {
                                napis += "  ";
                            }
                        }
                    }

                    Label row = new Label(napis, Label.CENTER);
                    row.setFont(new Font("Monospaced", Font.BOLD, 16));
                    row.setBackground(Color.darkGray);
                    row.setForeground(Color.white);
                    rowPanel.add(row);
                }

                // Repaint the panel to show the new rows
                rowPanel.revalidate();
                rowPanel.repaint();
            } catch (NumberFormatException ex) {
                showDialog("Błędny format liczby: " + input);
            } catch (Exception genEx) { // Catch other potential errors
                showDialog("Wystąpił błąd: " + genEx.getMessage());
            }
        }
    }

    // Helper method to show error messages in a dialog
    private void showDialog(String message) {
        Dialog errorDialog = new Dialog(this, "Błąd", true);
        errorDialog.setLayout(new FlowLayout());
        errorDialog.add(new Label(message));
        Button okButton = new Button("OK");
        okButton.addActionListener(ev -> errorDialog.dispose()); // Close dialog on OK click
        errorDialog.add(okButton);
        errorDialog.setSize(300, 100);
        errorDialog.setVisible(true);
    }

    public static void main(String[] args) {
        new Trojkat();
    }
}
