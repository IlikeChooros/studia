
import javafx.geometry.Point2D;
import javafx.scene.paint.Color;

public class Wolf extends Creature {

    /**
     * Create a wolf in the simluation
     * @param position
     * @param managerRef
     */
    public Wolf(Point2D position, Manager managerRef) {
        super(position, managerRef, Type.WOLF, WolfMovePolicies.getDefault());
    }

    /**
     * Create a wolf in the simluation with a custom move policy
     * @param position
     * @param managerRef
     * @param movePolicy
     */
    public Wolf(Point2D position, Manager managerRef, MovePolicy movePolicy) {
        super(position, managerRef, Type.WOLF, movePolicy);
    }

    /**
     * Makes a turn for the rabbit, depends on the 'wolfSpeed' setting
     */
    @Override
    public void cycle() throws InterruptedException {
        cycle(SParameters.wolfSpeed);
    }

    /**
     * Return wheter given type of creature can be captured,
     * by rabbit (no creature can be, so it always returns false)
     */
    @Override
    public boolean isCapture(Type t) {
        return t == Type.RABBIT;
    }

    /**
     * Get the color of the wolf (for UI)
     */
    @Override
    public Color getColor() {
        return Board.WOLF_COLOR;
    }
}
