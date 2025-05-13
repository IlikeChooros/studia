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

    public BaseShapeState(
        Color fillColor, Color strokeColor, 
        double strokeWidth, double rotation
    )
    {
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
            this.fillColor = null; // Or a default color
        }

        // Manually deserialize strokeColor
        double rStroke = in.readDouble();
        if (rStroke != -1.0) {
            double gStroke = in.readDouble();
            double bStroke = in.readDouble();
            double oStroke = in.readDouble();
            this.strokeColor = new Color(rStroke, gStroke, bStroke, oStroke);
        } else {
            this.strokeColor = null; // Or a default color
        }
    }
}

// Class for holding the position, rotation and visual data for the shape
class ShapeState extends BaseShapeState {
    private static final long serialVersionUID = 200L; // Different SUID from BaseShapeState
    public double startX, startY, endX, endY;
    
    public ShapeState(
        double startX, double startY, double endX, double endY,
        Color fillColor, Color strokeColor, double strokeWidth,double rotation
    )
    {
        super(fillColor, strokeColor, strokeWidth, rotation);
        this.startX = startX;
        this.startY = startY;
        this.endX = endX;
        this.endY = endY;
    }

    // Copy constructor
    public ShapeState(ShapeState other) {
        super(other);
        this.startX = other.startX;
        this.startY = other.startY;
        this.endX = other.endX;
        this.endY = other.endY;
    }
};