#!/bin/bash

# Configuration variables
JAVAFX_PATH="/opt/javafx-sdk-17.0.15/lib"
JAVAFX_MODULES="javafx.controls,javafx.fxml"
OUTPUT_DIR="classes"
MAIN_CLASS="Main"
RESOURCES_DIR="resources"

# Function to display error message and exit
error_exit() {
    echo "ERROR: $1" >&2
    exit 1
}

# Function to compile the project
compile() {
    echo "Creating output directory if needed..."
    mkdir -p "$OUTPUT_DIR" || error_exit "Failed to create output directory"
    
    echo "Compiling Java sources with JavaFX..."
    javac --module-path "$JAVAFX_PATH" --add-modules "$JAVAFX_MODULES" \
          -d "$OUTPUT_DIR" *.java || error_exit "Compilation failed"
    
    echo "Compilation successful!"
}

# Function to run the project
run() {
    echo "Running the application..."
    java --module-path "$JAVAFX_PATH" --add-modules "$JAVAFX_MODULES" \
         --class-path "$OUTPUT_DIR:$RESOURCES_DIR" "$MAIN_CLASS" || error_exit "Execution failed"
}

# Parse command line arguments
if [ "$1" == "clean" ]; then
    echo "Cleaning output directory..."
    rm -rf "$OUTPUT_DIR"
    exit 0
elif [ "$1" == "compile" ]; then
    compile
    exit 0
elif [ "$1" == "run" ]; then
    run
    exit 0
else
    # Default: compile and run
    compile
    run
fi