import java.util.LinkedList;
import java.util.PriorityQueue;

import javafx.geometry.Point2D;
import javafx.scene.paint.Color;

/**
 * Interface for creatures (in this case just for the wolves and rabbits)
 */
interface CreatureLike {
    public Color getColor();
    public Point2D getPosition();
    public void cycle();
    public Move genMove();
    public Creature.Type getType();
}

/**
 * Base class for all creatures, it is a thread, with random wait cycle
 */
abstract public class Creature implements Runnable, CreatureLike {
    
    /**
     * Type of the creature
     */
    static public enum Type {
        WOLF, RABBIT
    }

    /**
     * Data class for finding the closest creature
     */
    static protected class CreatureInfo implements Comparable<CreatureInfo> {
        int posDiff = 0;
        Creature creature = null;

        public CreatureInfo(int diff, Creature c) {
            this.posDiff = diff;
            this.creature = c;
        }

        @Override
        public int compareTo(CreatureInfo cmp) {
            return posDiff - cmp.posDiff;
        }
    };

    protected Point2D position;
    protected boolean isRunning;
    protected LinkedList<Creature> creaturesRef;
    protected Type type;

    public Creature(Point2D position) {
        this.position = position;
    }

    /**
     * Set refrence to all creatures in the enviorment
     */
    public void setCreatures(LinkedList<Creature> creatures) {
        this.creaturesRef = creatures;
    }

    @Override
    public void run() {
        while (isRunning) {
            try {
                cycle();
                Thread.sleep(Manager.getCycleDuration());
            } catch(InterruptedException e) {
                isRunning = false;
            }
        }
    }

    /**
     * Get the closest type of creature
     */
    protected CreatureInfo findClosest(Type type) {
        CreatureInfo creature = null;
        
        // Working in multi-thread enviorment,
        // the creatures may be modified (deleted),
        // by other threads
        synchronized (creaturesRef) {
            PriorityQueue<CreatureInfo> queue = new PriorityQueue<>();

            for (Creature c : creaturesRef) {
                if (c.type != type) {
                    continue;
                }

                Point2D target = c.getPosition();
                int diffX = Math.abs((int)(target.getX() - position.getX()));
                int diffY = Math.abs((int)(target.getY() - position.getY()));

                if (this == c || (diffX == 0 && diffY == 0)) {
                    continue;
                }

                // Get the square distance from the target, and put it on the queue
                queue.add(new CreatureInfo(Math.max(diffX, diffY), c));
            }

            // Get the closest creature
            CreatureInfo target = queue.poll();
            LinkedList<CreatureInfo> closest = new LinkedList<>();
            closest.add(target);

            // Add other creatures within the distance
            CreatureInfo other;
            while (target.posDiff == (other = queue.poll()).posDiff) {
                closest.add(other);
            }

            // Choose at random the target
            int targetIndex = 0;
            if (closest.size() != 1) {
                targetIndex = Manager.random.nextInt(closest.size());
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
        return this.position;
    }

    /**
     * Return enum type of the creature
     */
    public Type getType() {
        return type;
    }
}
