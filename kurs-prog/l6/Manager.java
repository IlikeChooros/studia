import java.util.LinkedList;
import java.util.concurrent.ThreadLocalRandom;

import javafx.application.Platform;
import javafx.geometry.Point2D;

public class Manager {
    private volatile LinkedList<Creature> creatures;
    private volatile LinkedList<Thread> threads;
    private volatile Creature[][] occupancy;
    private final Board uiBoard;

    // Templated function to populate the creatures of given type
    private <E extends Creature> void populate(Class<E> clazz, int count) {
        for (int i = 0; i < count; i++) {
            try {
                E c = clazz.getConstructor(Point2D.class, Manager.class).newInstance(getRandomPoint(), this);
                c.setCreatures(creatures);
                creatures.add(c);
                threads.add(new Thread(c));
                occupancy[c.y()][c.x()] = c;
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public Manager() {
        uiBoard = new Board(SParameters.nRows, SParameters.nCols, 20);
        creatures = new LinkedList<>();
        threads = new LinkedList<>();
        occupancy = new Creature[SParameters.nRows][SParameters.nCols];

        // Set all occupancy to NONE
        for (int r = 0; r < SParameters.nRows; r++) {
            for (int c = 0; c < SParameters.nCols; c++) {
                occupancy[r][c] = null;
            }
        }

        // Based on parameters, make the creatures
        populate(Rabbit.class, SParameters.rabbitCount);
        populate(Wolf.class, SParameters.wolfCount);
        
        for (Thread t : threads) {
            t.start();
        }
    }

    /**
     * Kill all creatures
     */
    public void kill() {
        for (Thread t : threads) {
            t.interrupt();
            try {
                t.join();
            } 
            catch (InterruptedException e) {}
        }
    }

    // Get the ui board representation
    public Board getUIBoard() {
        return uiBoard;
    }

    // Get random cycle duration
    public static long getCycleDuration() {
        return (long)((ThreadLocalRandom.current().nextDouble() + 0.5) * SParameters.cycleRate);
    }

    /**
     * Get the creature type at given position
     */
    public Creature at(Point2D position) {
        return occupancy[(int)position.getY()][(int)position.getX()];
    }

    /**
     * Generate a radom point that is not occupied yet on the board
     */
    private Point2D getRandomPoint() {
        ThreadLocalRandom random = ThreadLocalRandom.current();

        int x, y;
        do {
            x = random.nextInt(SParameters.nCols);
            y = random.nextInt(SParameters.nRows);
        } while (occupancy[y][x] != null);

        return new Point2D(x, y);
    }

    /**
     * Check if given point is within the board
     */
    public static boolean inBounds(Point2D position) {
        int x = (int)position.getX();
        int y = (int)position.getY();

        if (x < 0 || x >= SParameters.nCols) {
            return false;
        }
        if (y < 0 || y >= SParameters.nRows) {
            return false;
        }

        return true;
    }

    /**
     * Update the board UI, and creature coordinates, will be called
     * by the creatures
     */
    public void makeMove(Move m, Creature moved) {
        Point2D from = m.getFrom();
        Point2D to = m.getTo();
        
        // Ensure 'moved.position' is updated safely if read by other threads
        // without synchronization on 'moved' object.
        // The synchronized block here protects the 'position' field during update.
        synchronized (moved) {
            moved.setPosition(to);
        }

        // Update occupancy
        synchronized(occupancy) {
            // Remove from the previous square
            occupancy[(int)from.getY()][(int)from.getX()] = null;
            occupancy[(int)to.getY()][(int)to.getX()] = moved;
        }

        // If that's a capture, update the lists
        if (m.getType() == Move.Type.CAPTURE) {
            synchronized (creatures) { // creatures is LinkedList, ensure this is the correct lock
                Creature eatenCreature = m.getEaten();
                int index = -1;
                for (int i = 0; i < creatures.size(); i++) {
                    if (creatures.get(i) == eatenCreature) {
                        index = i;
                        break;
                    }
                }

                if (index != -1) {
                    Thread t = threads.get(index);
                    t.interrupt(); // signal the creature thread to stop

                    // Remove it from the lists
                    creatures.remove(index);
                    threads.remove(index);
                }
            }
        }

        // UI update (must be on JavaFX thread)
        Platform.runLater(() -> {
            uiBoard.setColor((int)from.getX(), (int)from.getY(), Board.EMPTY_COLOR);
            uiBoard.setColor((int)to.getX(), (int)to.getY(), moved.getColor());
        });
    }
}
