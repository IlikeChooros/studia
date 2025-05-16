import javafx.scene.paint.Color;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

// Base class for shape states
class BaseShapeState implements Serializable {
    private static final long serialVersionUID = 100L;
    public transient Color fillColor; // Color is not serializable
    public transient Color strokeColor;
    public double strokeWidth;
    public double rotation;
    public double centerX, centerY;

    public BaseShapeState(
        double centerX, double centerY,
        Color fillColor, Color strokeColor, 
        double strokeWidth, double rotation
    )
    {
        this.centerX = centerX;
        this.centerY = centerY;
        this.fillColor = fillColor;
        this.strokeColor = strokeColor;
        this.strokeWidth = strokeWidth;
        this.rotation = rotation;
    }

    public BaseShapeState(BaseShapeState other) {
        this.fillColor = other.fillColor; // I might need to change that
        this.strokeColor = other.strokeColor;
        this.strokeWidth = other.strokeWidth;
        this.rotation = other.rotation;
        this.centerX = other.centerX;
        this.centerY = other.centerY;
    }

    // Custom serialization method
    private void writeObject(ObjectOutputStream out) throws IOException {
        out.defaultWriteObject(); // Writes non-transient fields

        // Manually serialize fillColor
        if (fillColor != null) {
            out.writeDouble(fillColor.getRed());
            out.writeDouble(fillColor.getGreen());
            out.writeDouble(fillColor.getBlue());
            out.writeDouble(fillColor.getOpacity());
        } else {
            out.writeDouble(-1.0); // Sentinel for null red
        }


        // Manually serialize strokeColor
        if (strokeColor != null) {
            out.writeDouble(strokeColor.getRed());
            out.writeDouble(strokeColor.getGreen());
            out.writeDouble(strokeColor.getBlue());
            out.writeDouble(strokeColor.getOpacity());
        } else {
            out.writeDouble(-1.0); // Sentinel for null red
        }
    }

    // Custom deserialization method
    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
        in.defaultReadObject(); // Reads non-transient fields

        // Manually deserialize fillColor
        double rFill = in.readDouble();
        if (rFill != -1.0) {
            double gFill = in.readDouble();
            double bFill = in.readDouble();
            double oFill = in.readDouble();
            this.fillColor = new Color(rFill, gFill, bFill, oFill);
        } else {
            this.fillColor = null;
        }

        // Manually deserialize strokeColor
        double rStroke = in.readDouble();
        if (rStroke != -1.0) {
            double gStroke = in.readDouble();
            double bStroke = in.readDouble();
            double oStroke = in.readDouble();
            this.strokeColor = new Color(rStroke, gStroke, bStroke, oStroke);
        } else {
            this.strokeColor = null;
        }
    }
}