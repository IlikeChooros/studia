import javafx.geometry.Point2D;
import javafx.scene.paint.Color;

public class Rabbit extends Creature {

    /**
     * Create a rabbit in the simluation
     * @param position
     * @param managerRef
     */
    public Rabbit(Point2D position, Manager managerRef) {
        super(position, managerRef, Type.RABBIT, new RabbitMovePolicies.AlwaysRunAwayFromWolves());
    }

    public Rabbit(Point2D posisiton, Manager managerRef, MovePolicy.Policies movepolicy) {
        super(posisiton, managerRef, Type.RABBIT, AllMovementPolicies.get(movepolicy));
    }

    /**
     * Makes a turn for the rabbit, depends on the 'rabbitSpeed' setting
     */
    @Override
    public void cycle() throws InterruptedException {
        cycle(SParameters.rabbitSpeed);
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
