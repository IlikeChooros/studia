import java.util.Comparator;
import java.util.LinkedList;
import java.util.PriorityQueue;
import java.util.Vector;
import java.util.concurrent.ThreadLocalRandom;

import javafx.application.Platform;
import javafx.geometry.Point2D;
import javafx.scene.paint.Color;
import javafx.util.Pair;

/**
 * Interface for creatures (in this case just for the wolves and rabbits),
 * Each creature should define:
 * - public Move genMove() -> generate a move it's turn
 * - public Color getColor() -> get the color of the creature
 */
interface CreatureLike {

    /**
     * Generate the move in 1 cycle, will either make:
     * - random move
     * - run towrads closest creature (for example rabbit)
     * - run away (ex. from wolf)
     */
    public Move genMove();

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
    protected LinkedList<Creature> creaturesRef;
    protected Type type;
    protected Manager manager;

    public Creature(Point2D position, Manager manager, Type type) {
        this.position = position;
        this.manager = manager;
        this.isRunning = true;
        this.type = type;
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
            Move move = genMove();

            if (move != null) {
                manager.makeMove(move, this);
            }
            
            Thread.sleep(cycleDuration / nCycyles);
        }
    }

    /**
     * Generate a move based on the closest target creature
     * @param info target creature
     * @param type either move towards it, or run away from it
     * @return generated move
     */
    protected Move genMoveType(CreatureInfo info, MoveType type) {
       
        Point2D currentPos = getPosition();

        // Add all possible moves
        Vector<Move> moves = new Vector<>(8);
        Vector<Move> validMoves = new Vector<>(8);
        boolean capturePossible = false;

        double diffX[] = {-1, 0, 1, -1, 1, -1,  0,  1};
        double diffY[] = { 1, 1, 1,  0, 0, -1, -1, -1};

        for(int i = 0; i < diffX.length; i++) {
            Point2D to = currentPos.add(diffX[i], diffY[i]);

            // Check if the point is within bounds
            if (!Manager.inBounds(to)) {
                continue;
            }

            // at is already synchronized, so no need to synchronize it again
            Creature c = manager.at(to);

            // No occupant at this square
            if (c == null) {
                moves.add(new Move(currentPos, to));
                continue;
            }

            // Occupied by the same type, ignore it
            if (c.getType() == getType()) {
                continue;
            }

            // Check if I can capture this creature
            if (isCapture(c.getType())) {
                moves.add(new Move(currentPos, to, c));
                capturePossible = true;
            }
        }

        switch (type) {
            case TOWARDS:
                // Simply check if the distance between the target and
                // given move shrinks
                for (Move m : moves) {
                    // If there is a capture, make it the only move
                    if (capturePossible) {
                        if (m.getType() == Move.Type.CAPTURE){
                            validMoves.add(m);
                        }
                    } 
                    else if (distance(positionDiff(m.getTo(), info.creature.getPosition())) < info.posDiff) {
                        validMoves.add(m);
                    }
                }
                break;
            
            case AWAY:
                // If the distance increases, add it to the valid moves
                Vector<Move> keepSameDist = new Vector<>(8);
                for (Move m : moves) {
                    int dist = distance(positionDiff(m.getTo(), info.creature.getPosition()));

                    if (dist > info.posDiff) {
                        validMoves.add(m);
                    }
                    else if (dist == info.posDiff) {
                        keepSameDist.add(m);
                    }
                }

                // Check if ideal moves are not possible,
                // then keep the same distance
                if (validMoves.isEmpty()) {
                    validMoves = keepSameDist;
                }

                break;
            case RANDOM:
            default: 
                // All possible moves are valid
                validMoves = moves;
                break;
        }

        // No valid moves
        if (validMoves.isEmpty()) {
            return null;
        }

        // Only 1, so there is no need to randomly select one
        if (validMoves.size() == 1) {
            return validMoves.firstElement();
        }
        
        // Choose randomly
        return validMoves.get(ThreadLocalRandom.current().nextInt(validMoves.size()));
    }

    /**
     * Get the abs. difference from given point (1st x, 2nd y)
     */
    static protected Pair<Integer, Integer> positionDiff(Point2D p1, Point2D p2) {
        return new Pair<Integer,Integer>(
            Math.abs((int)(p1.getX() - p2.getX())),
            Math.abs((int)(p1.getY() - p2.getY())) 
        );
    }

    /**
     * Get the distance from given point
     */
    static protected int distance(Pair<Integer, Integer> dist) {
        return Math.max(dist.getKey(), dist.getValue());
    }

    /**
     * Get the closest type of creature, if many withing the same range,
     * select at random
     */
    protected CreatureInfo findClosest(Type type, int maxRange) {
        CreatureInfo creature = null;

        // Working in multi-thread enviorment,
        // the creatures may be modified (deleted),
        // by other threads
        synchronized (creaturesRef) {
            Comparator<CreatureInfo> cmp = new CreatureInfoComparator();
            PriorityQueue<CreatureInfo> queue = new PriorityQueue<>(creaturesRef.size(), cmp);

            for (Creature c : creaturesRef) {
                if (c.type != type) {
                    continue;
                }

                Pair<Integer, Integer> diffs = positionDiff(c.getPosition(), position);
                int dist = distance(diffs);
                if (this == c || (dist == 0) || (dist > maxRange)) {
                    continue;
                }

                // Get the square distance from the target, and put it on the queue
                queue.add(new CreatureInfo(dist, c));
            }

            // No creatures of given type
            if (queue.isEmpty()) {
                return null;
            }

            // Get the closest creature
            CreatureInfo target = queue.poll();
            LinkedList<CreatureInfo> closest = new LinkedList<>();
            closest.add(target);

            // Add other creatures within the distance
            CreatureInfo other;
            while (!queue.isEmpty() && target.posDiff == (other = queue.poll()).posDiff) {
                closest.add(other);
            }

            // Choose at random the target
            int targetIndex = 0;
            if (closest.size() != 1) {
                targetIndex = ThreadLocalRandom.current().nextInt(closest.size());
            }

            // Set the target
            creature = closest.get(targetIndex);
        }

        return creature;
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
