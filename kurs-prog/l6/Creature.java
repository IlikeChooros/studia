import java.util.Comparator;
import java.util.LinkedList;
import java.util.PriorityQueue;
import java.util.Vector;
import java.util.concurrent.ThreadLocalRandom;

import javafx.application.Platform;
import javafx.geometry.Point2D;
import javafx.scene.paint.Color;

/**
 * Interface for creatures (in this case just for the wolves and rabbits),
 * Each creature should define:
 * - public Move genMove() -> generate a move it's turn
 * - public Color getColor() -> get the color of the creature
 */
interface CreatureLike {

    /**
     * Get the color of the creature
     */
    public Color getColor();

    /**
     * Check if given type of creature can be captured,
     * (for example for wolf: RABBIT is valid)
     */
    public boolean isCapture(Creature.Type t);


    // ---------- DEFINED IN CREATURE CLASS ----------

    /**
     * Get position of the creature on the 2D board
     */
    public Point2D getPosition();

    /**
     * Set the position of the creature
     */
    public void setPosition(Point2D pos);

    /**
     * Make one turn of movement
     */
    public void cycle() throws InterruptedException;

    /**
     * Get the type of the creature
     */
    public Creature.Type getType();

    /**
     * Get X coordinate of the creature, but as integer
     */
    public int x();

    /**
     * Get Y coordinate of the creature, but as integer
     */
    public int y();
}



/**
 * Base class for all creatures, it is a thread, with random wait cycle
 */
abstract public class Creature implements Runnable, CreatureLike {
    
    /**
     * Type of the creature
     */
    static public enum Type {
        WOLF, RABBIT, NONE
    }

    /**
     * Type of move to make, either move towards 
     * given creature, or run away from it
     */
    static protected enum MoveType {
        TOWARDS, AWAY, RANDOM
    };

    static interface MoveGenerator {
        /**
         * Generate a move for the creature
         * @return generated move
         */
        public Move genMove();  
    }

    /**
     * Data class for finding the closest creature
     */
    static protected class CreatureInfo {
        int posDiff = 0;
        Creature creature = null;

        public CreatureInfo(int diff, Creature c) {
            this.posDiff = diff;
            this.creature = c;
        }
    };

    static protected class CreatureInfoComparator implements Comparator<CreatureInfo> {
        @Override
        public int compare(CreatureInfo x, CreatureInfo y) {
            // negative -> x < y (so x should be in front)
            // zero -> x == y (are the same)
            // positive -> x > y (so y should be in front of the queue)
            return x.posDiff - y.posDiff;
        }
    }

    protected volatile Point2D position;
    protected volatile boolean isRunning = false;
    protected MovePolicy policy = null;
    protected LinkedList<Creature> creaturesRef;
    protected Type type;
    protected Manager manager;

    public Creature(Point2D position, Manager manager, Type type, MovePolicy policy) {
        this.position = position;
        this.manager = manager;
        this.isRunning = true;
        this.type = type;
        this.policy = policy;
        this.policy.setOwner(this);
    }

    /**
     * Set refrence to all creatures in the enviorment
     */
    public void setCreatures(LinkedList<Creature> creatures) {
        this.creaturesRef = creatures;
    }

    @Override
    public void run() {
        // Add it self to the simulation (simply set the color on it's coordinates)
        // It is illegal to modify the JavaFX components in child threads (no the main JavaFX thread)
        // So this is needed, rather than simply calling the 'setColor'
        Platform.runLater(() -> {
            manager.getUIBoard().setColor((int)position.getX(), (int)position.getY(), getColor());
        });

        while (isRunning) {
            try {
                cycle();
            } catch(InterruptedException e) {
                isRunning = false;
            }
        }
    }

    /**
     * Makes 1 turn for the creature
     */
    @Override 
    public void cycle() throws InterruptedException {
        cycle(1);
    }


    /**
     * Makes 'nCycles' turns for the creature to make it's move,
     * simply calls 'genMove' and then 'makeMove' on the manager
     */
    protected void cycle(int nCycyles) throws InterruptedException {
        // genMove will use probably `findClosest` and
        // then make a decision to move (either towards it or run away from it)
        long cycleDuration = Manager.getCycleDuration();
        for (int i = 0; i < nCycyles; i++) {
            Move move = policy.genMove();

            if (move != null) {
                manager.makeMove(move, this);
            }
            
            Thread.sleep(cycleDuration / nCycyles);
        }
    }

    /**
     * Get the position of the creature (x,y)
     */
    @Override 
    public Point2D getPosition() {
        synchronized (this) {
            return position;
        }
    }

    /**
     * Set the position of the creature
     */
    @Override
    public void setPosition(Point2D pos) {
        synchronized (this) {
            this.position = pos;
        }
    }

    /**
     * Get x position as int
     */
    @Override
    public int x() {
        synchronized (this) {
            return (int)position.getX();
        }
    }

    /**
     * Get y position as int
     */
    @Override
    public int y() {
        synchronized (this) {
            return (int)position.getY();
        }
    }

    /**
     * Return enum type of the creature
     */
    @Override
    public Type getType() {
        return type;
    }
}
