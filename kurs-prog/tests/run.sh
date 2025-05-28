#!/bin/bash

JAVAFX_PATH="/opt/javafx-sdk-17.0.15/lib"
JAVAFX_MODULES="javafx.controls,javafx.fxml"
OUTPUT_DIR="classes"
MAIN_CLASS="Main"

error_exit() {
	echo "Error: $1" >$2
	exit 1
}

compile() {
	echo "Compiling the javafx application..."
	javac --module-path "$JAVAFX_PATH" --add-modules "$JAVAFX_MODULES" \
		-d "$OUTPUT_DIR" *.java || error_exit "Compilation failed"
	echo "Compilation successfull"
}

run() {
	echo "Running the $MAIN_CLASS"
	java --module-path "$JAVAFX_PATH" --add-modules "$JAVAFX_MODULES" \
		--class-path "$OUTPUT_DIR" "$MAIN_CLASS" || error_exit "Execution failed"
}

if [ "$1" == "run" ]; then
	MAIN_CLASS="$2"
	run
	exit 0
elif [ "$1" == "compile" ]; then
	compile
	exit 0
else
	# Default, compile and run
	MAIN_CLASS="$1"
	compile
	run
fi
