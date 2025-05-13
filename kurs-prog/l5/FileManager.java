import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import javafx.stage.FileChooser;
import javafx.stage.Stage;

public class FileManager {

    /**
     * Data class to hold all information needed for saving and loading.
     * Must be public static to be accessible from other classes like PaintToolBar.
     */
    public static class DrawingData implements Serializable {
        private static final long serialVersionUID = 20231115L; // Use a meaningful serialVersionUID
        private final LinkedList<BShape> shapes;
        private final List<Integer> history;
        private final int historyIdCounter;
        private final String filename;

        public DrawingData(LinkedList<BShape> shapes, List<Integer> history, int historyIdCounter, String filename) {
            // Store copies to ensure immutability of this data object after creation if needed,
            // though for serialization, direct references are fine.
            this.shapes = new LinkedList<>(shapes);
            this.history = new ArrayList<>(history);
            this.historyIdCounter = historyIdCounter;
            this.filename = filename;
        }

        public LinkedList<BShape> getShapes() { 
            return new LinkedList<>(shapes); 
        }

        public List<Integer> getHistory() { 
            return new ArrayList<>(history); 
        }

        public int getHistoryIdCounter() { 
            return historyIdCounter; 
        }

        public String getFilename() {
            return filename;
        }
    }

    public static final String FILE_EXTENSION = ".pfx";

    /**
     * Saves the current state of the drawing board to a file.
     * @param drawingBoard The DrawingBoard instance containing the data to save.
     * @param stage The primary stage, used to show the FileChooser dialog.
     */
    public static String save(DrawingBoard drawingBoard, Stage stage) {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Save Drawing");
        fileChooser.getExtensionFilters().addAll(
                new FileChooser.ExtensionFilter("Drawing Files (*" + FILE_EXTENSION + ")", "*" + FILE_EXTENSION),
                new FileChooser.ExtensionFilter("All Files (*.*)", "*.*"));
        File file = fileChooser.showSaveDialog(stage);

        if (file != null) {
            String filePath = file.getAbsolutePath();
            if (!filePath.toLowerCase().endsWith(FILE_EXTENSION)) {
                file = new File(filePath + FILE_EXTENSION);
            }

            try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(file))) {
                DrawingData data = new DrawingData(
                    drawingBoard.getShapes(),
                    drawingBoard.getHistory(),
                    drawingBoard.getHistoryIdCounter(),
                    file.getName()
                );
                oos.writeObject(data);
                System.out.println("Drawing saved to " + file.getAbsolutePath());
            } catch (IOException e) {
                e.printStackTrace();
                // Consider showing an error Alert to the user
            }
        }
        return file != null ? file.getName() : "";
    }

    /**
     * Loads a drawing from a file.
     * @param stage The primary stage, used to show the FileChooser dialog.
     * @return DrawingData object if successful, null otherwise.
     */
    public static DrawingData load(Stage stage) {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Load Drawing");
        fileChooser.getExtensionFilters().addAll(
                new FileChooser.ExtensionFilter("Drawing Files (*" + FILE_EXTENSION + ")", "*" + FILE_EXTENSION),
                new FileChooser.ExtensionFilter("All Files (*.*)", "*.*"));
        File file = fileChooser.showOpenDialog(stage);

        if (file != null) {
            try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(file))) {
                Object loadedObject = ois.readObject();
                if (loadedObject instanceof DrawingData) {
                    System.out.println("Drawing loaded from " + file.getAbsolutePath());
                    return (DrawingData) loadedObject;
                } else {
                    System.err.println("Error: Loaded file does not contain valid drawing data.");
                    // Consider showing an error Alert to the user
                }
            } catch (IOException | ClassNotFoundException e) {
                e.printStackTrace();
                // Consider showing an error Alert to the user
            }
        }
        return null;
    }
}
