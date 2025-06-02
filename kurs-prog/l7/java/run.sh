#!/bin/bash

# Configuration variables
JAVAFX_PATH="/opt/javafx-sdk-17.0.15/lib"
JAVAFX_MODULES="javafx.controls,javafx.fxml"
OUTPUT_DIR="classes"
MAIN_CLASS="Main"
RESOURCES_DIR="resources"
JAVA_OPTIONS=""

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

    if [ "$JAVA_OPTIONS" != "" ]; then
        echo "Using Java options: $JAVA_OPTIONS"

        java --module-path "$JAVAFX_PATH" --add-modules "$JAVAFX_MODULES" \
         --class-path "$OUTPUT_DIR:$RESOURCES_DIR" \
            "$JAVA_OPTIONS" \
         "$MAIN_CLASS" || error_exit "Execution failed"
    else
        echo "No special Java options provided."

        java --module-path "$JAVAFX_PATH" --add-modules "$JAVAFX_MODULES" \
         --class-path "$OUTPUT_DIR:$RESOURCES_DIR" \
         "$MAIN_CLASS" || error_exit "Execution failed"
    fi
    
}

# Parse command line arguments
if [ "$1" == "clean" ]; then
    echo "Cleaning output directory..."
    rm -rf "$OUTPUT_DIR"
    exit 0
elif [ "$1" == "compile" ]; then
    echo "Compiling the project..."
    # compile
    exit 0
elif [ "$1" == "run" ]; then

    # Add supprot for verbose or gc-verbose options
    if [ "$2" == "verbose" ]; then
        JAVA_OPTIONS="-Xlint:all"
        echo "Verbose mode enabled for compilation."
    elif [ "$2" == "gc-verbose" ]; then
        JAVA_OPTIONS="-verbose:gc"
        echo "GC verbose mode enabled for compilation."
    else
        JAVA_OPTIONS=""
        echo "No special options for compilation."
    fi

    run
    exit 0
elif [ "$1" == "help" ]; then

    echo "Usage: $0 [clean|compile|run [verbose|gc-verbose]|help]"
    echo "  clean          - Clean the output directory"
    echo "  compile        - Compile the Java sources"
    echo "  run [verbose|gc-verbose] - Run the application with optional Java options"
    echo "  help           - Display this help message"
    exit 0

else
    # Default: compile and run
    compile
    run
fi