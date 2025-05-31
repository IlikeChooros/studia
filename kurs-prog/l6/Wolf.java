import javafx.scene.paint.Color;

public class Wolf extends Creature {

    /**
     * Create a wolf in the simluation
     * @param position
     * @param managerRef
     */
    public Wolf(Position position, Manager managerRef) {
        super(position, managerRef, Type.WOLF, new WolfMovePolicies.RunTowardsRabbits());
    }

    /**
     * Create a wolf in the simluation with a custom move policy
     * @param position
     * @param managerRef
     * @param movePolicy
     */
    public Wolf(Position position, Manager managerRef, MovePolicy.Policies movePolicy) {
        super(position, managerRef, Type.WOLF, AllMovementPolicies.get(movePolicy));
    }

    /**
     * Makes a turn for the wolf, depends on the 'wolfSpeed' setting
     */
    @Override
    public void cycle() throws InterruptedException {
        cycle(SParameters.wolfSpeed);
    }

    /**
     * Return wheter given type of creature can be captured,
     * by wolf (right now only rabbit can be captured)
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
