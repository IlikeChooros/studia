
/**
 * Class representing a move on the board, holds:
 * - from position
 * - to postion
 * - eaten creature
 * - type of the move
 */
public class Move implements CachableItem<Move> {
    
    public static enum Type {
        SILENT,
        CAPTURE
    }

    private Position from;
    private Position to;
    private Creature eaten;
    private Type type;

    public Move() {
        this.setAllFields(null, null, null);
    }

    public Move(Position from, Position to, Creature creature) {
        this.setAllFields(from, to, creature);
    }

    public Move(Position from, Position to) {
        this.setAllFields(from, to, null);
    }

    public void setAllFields(Object... args) {
        this.from = (Position)args[0];
        this.to = (Position)args[1];
        eaten = (Creature)args[2];
        type = eaten != null ? Type.CAPTURE : Type.SILENT;
    }

    public void copy(Move m) {
        this.eaten = m.eaten;
        // this.from.set(m.from.getX(), m.);
        this.to = m.to;
        this.from = m.from;
        this.type = m.type;
    }

    /**
     * Get starting position
     */
    public Position getFrom() {
        return from;
    }

    /**
     * Get the end coordiantes
     */
    public Position getTo() {
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
