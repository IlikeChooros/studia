import javafx.geometry.Point2D;

/**
 * Class representing a move on the board, holds:
 * - from position
 * - to postion
 * - eaten creature
 * - type of the move
 */
public class Move {
    
    static public enum Type {
        SILENT,
        CAPTURE
    }

    private final Point2D from;
    private final Point2D to;
    private final Creature eaten;
    private final Type type;

    public Move(Point2D from, Point2D to, Creature creature) {
        eaten = creature;
        type = creature != null ? Type.CAPTURE : Type.SILENT;
        this.from = from;
        this.to = to;
    }

    public Move(Point2D from, Point2D to) {
        this(from, to, null);
    }

    /**
     * Get starting position
     */
    public Point2D getFrom() {
        return from;
    }

    /**
     * Get the end coordiantes
     */
    public Point2D getTo() {
        return to;
    }

    /**
     * Get type of the move (either SILENT or CAPTURE)
     */
    public Type getType() {
        return type;
    }

    /**
     * Get eaten creature (may be null)
     */
    public Creature getEaten() {
        return eaten;
    }
}
