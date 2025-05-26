import java.util.LinkedList;
import java.util.Random;

import javafx.geometry.Point2D;
import javafx.scene.paint.Paint;

public class Manager {
    private volatile LinkedList<Creature> creatures;
    private volatile LinkedList<Thread> threads;
    static public Random random = new Random();
    private final Board uiBoard;

    public Manager() {
        uiBoard = new Board(SParameters.nRows, SParameters.nCols, 20);

        // Based on parameters, make the creatures
    }

    // Get the ui board representation
    public Board getUIBoard() {
        return uiBoard;
    }

    // Get random cycle duration
    public static long getCycleDuration() {
        return (long)((random.nextDouble() + 0.5) * SParameters.cycleRate);
    }

    /**
     * Update the board UI, assuming the creature already update it's position
     */
    public void makeMove(Move m) {
        Point2D from = m.getFrom();
        Point2D to = m.getTo();
        Paint color = uiBoard.getColor((int)from.getX(), (int)from.getY());

        // If that's a capture, update the lists
        if (m.getType() == Move.Type.CAPTURE) {
            synchronized (creatures) {
                int index = creatures.indexOf(m.getEaten());
                Thread t = threads.get(index);
                t.interrupt(); // kill the creature

                // Remove it from the lists
                creatures.remove(index);
                threads.remove(index);
            }
        }

        uiBoard.setColor((int)from.getX(), (int)from.getY(), Board.EMPTY_COLOR);
        uiBoard.setColor((int)to.getX(), (int)to.getY(), color);
    }
}
