# Paint in JavaFX 

Still better than MS Paint...

## Features
- **Drawing Shapes**: Draw various shapes like circles, rectangles, lines, polygons, etc.
- **Color Selection**: Choose colors for shapes and the background.
- **Shape Selection**: Select shapes to move or delete them.
- **Undo/Redo**: Undo and redo actions.
- **Save/Load**: Save the drawing to a file and load it back.

## Requirements
- Java 17 or higher
- JavaFX SDK (17.0.15 or higher)


## Run the app

Running on linux (Ubuntu 22.04) with Java 17 and JavaFX 17.0.15.
1. Download JavaFX SDK from [Gluon](https://gluonhq.com/products/javafx/)
2. Unzip the SDK to a directory, e.g., `/opt/javafx-sdk-17.0.15/`

Compile with 1 *simple* command:

```sh
javac --module-path /opt/javafx-sdk-17.0.15/lib --add-modules javafx.controls Main.java DrawingBoard.java PaintToolBar.java StatusBar.java Shapes.java Circle.java Rectangle.java Line.java NPolygon.java Triangle.java Pentagon.java Hexagon.java -d classes
```

```sh
cd classes && java --module-path /opt/javafx-sdk-17.0.15/lib --add-modules javafx.controls,javafx.fxml,javafx.graphics,javafx.media,javafx.swing,javafx.web Main && cd ..
```

