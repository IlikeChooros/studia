
/**
 * Optimized class for no-copy of the 'moves'.
 * Pre-allocates the spaces for the moves (8), then
 * modifies the moves, by changing it's from,to,eaten variables
 */
public class MoveList extends CachedList<Move> {
    
    public MoveList() {
        super(Move.class, 8);
    }
}
