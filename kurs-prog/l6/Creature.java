import javafx.geometry.Point2D;
import javafx.scene.paint.Color;

interface CreatureLike {
    public Color getColor();
    public Point2D getPosition();
    public void cycle();
}

abstract public class Creature implements Runnable, CreatureLike {
    protected double cycleRate;
    protected boolean isRunning;

    public Creature(double cycleRate) {
        this.cycleRate = cycleRate;
    }

    @Override
    public void run() {
        while (isRunning) {
            cycle();
            try {
                wait((long)cycleRate);
            } catch(InterruptedException e) {
                isRunning = false;
            }
        }
    }
}
