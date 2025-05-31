import java.util.LinkedList;

import javafx.application.Platform;
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
    public Position getPosition();

    /**
     * Set the position of the creature
     */
    public void setPosition(Position pos);

    /**
     * Make one turn of movement
     */
    public void cycle() throws InterruptedException;

    /**
     * Kill the creature
     */
    public void kill();

    /**
     * Put the creature to sleep
     */
    public void suspend(int cycles);

    /**
     * Wake up the creature
     */
    public void resume();

    /**
     * Get suspended state
     */
    public boolean getSuspended();

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
    public static enum Type {
        WOLF, RABBIT, NONE
    }

    static interface MoveGenerator {
        /**
         * Generate a move for the creature
         * @return generated move
         */
        public Move genMove();  
    }

    protected volatile Position position;
    protected volatile boolean isRunning = false;
    protected volatile boolean isSuspended = false;
    protected volatile int nSuspendCycles = 0;
    protected MovePolicy policy = null;
    protected LinkedList<Creature> creaturesRef;
    protected Type type;
    protected Manager manager;

    /**
     * Create a new creature with given position, type and move policy.
     * By default all creatures are suspended at first, user should call 
     * 'resume'to activate them
     */
    public Creature(Position position, Manager manager, Type type, MovePolicy policy) {
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

                // Suspended state
                if (isSuspended) {
                    synchronized(this) {
                        // Wait forever (until 'resume')
                        while(nSuspendCycles == -1) {
                            wait();
                        }

                        // Wait for specified number of cycles
                        if (nSuspendCycles > 0) {
                            wait(Manager.getCycleDuration() * nSuspendCycles);
                            nSuspendCycles = 0;
                            isSuspended = false;
                        }
                    }
                }

            } catch(InterruptedException e) {
                isRunning = false;
            }
        }
    }

    /**
     * Kills the creature
     */
    @Override
    public synchronized void kill() {
        isRunning = false;
    }

    /**
     * Puts the creature to sleep, for given ncycles,
     * (if == -1, then wait's for the 'resume')
     */
    @Override
    public synchronized void suspend(int ncycles) {
        isSuspended = true;
        nSuspendCycles = ncycles;
        notify();
    }

    /**
     * Resumes the creature from the sleep state
     */
    @Override
    public synchronized void resume() {
        isSuspended = false;
        nSuspendCycles = 0;
        notify();
    }

    /**
     * Get the suspended state
     */
    @Override
    public synchronized boolean getSuspended() {
        return isSuspended;
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
        for (int i = 0; (i < nCycyles) && !isSuspended; i++) {
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
    public synchronized Position getPosition() {
        return position;
    }

    /**
     * Set the position of the creature
     */
    @Override
    public synchronized void setPosition(Position pos) {
        this.position = pos;
    }

    /**
     * Get x position as int
     */
    @Override
    public synchronized int x() {
        return (int)position.getX();
    }

    /**
     * Get y position as int
     */
    @Override
    public synchronized int y() {
        return (int)position.getY();
    }

    /**
     * Return enum type of the creature
     */
    @Override
    public Type getType() {
        return type;
    }
}
