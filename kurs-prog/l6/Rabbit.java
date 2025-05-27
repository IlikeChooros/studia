import javafx.geometry.Point2D;
import javafx.scene.paint.Color;

public class Rabbit extends Creature {

    /**
     * Create a rabbit in the simluation
     * @param position
     * @param managerRef
     */
    public Rabbit(Point2D position, Manager managerRef) {
        super(position, managerRef, Type.RABBIT);
    }

    /**
     * Generates a move for the rabbit, runs away from the closest wolf,
     * if there is no wolves, walks randomly
     */
    @Override
    public Move genMove() {
        CreatureInfo closest = findClosest(Type.WOLF);

        // No target creatures
        if (closest == null) {
            return genMoveType(null, MoveType.RANDOM);
        }

        return genMoveType(closest, MoveType.AWAY);        
    }

    /**
     * Return wheter given type of creature can be captured,
     * by rabbit (no creature can be, so it always returns false)
     */
    @Override
    public boolean isCapture(Type t) {
        return false;
    }


    @Override
    public Color getColor() {
        return Board.RABBIT_COLOR;
    }
}
