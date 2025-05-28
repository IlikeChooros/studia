import javafx.geometry.Point2D;

public class RabbitMovePolicies {
    
    static public class AlwaysRunAwayFromWolves extends MovePolicy {
        /**
         * Generates a move for the rabbit, runs away from the closest wolf,
         * if there is no wolves, walks randomly
         */
        @Override
        public Move genMove() {
            Creature.CreatureInfo closest = findClosest(Creature.Type.WOLF, SParameters.rabbitRange);

            // No target creatures
            if (closest == null) {
                return genMoveType(null, Creature.MoveType.RANDOM);
            }

            return genMoveType(closest, Creature.MoveType.AWAY);        
        }

    }

    // Movement as explained in the problem
    public static class ProblemMovement extends MovePolicy {
        @Override
        public Move genMove() {
            /*
            Zając zawsze ucieka w kierunku przeciwnym do wilka. Jeżeli pole
            na które chciałby przejść jest zajęte to nie rusza się, a jeżli do chodzi do ściany to wy-
            biera z rozkładem jednostajnym jeden z pięciu dozwolonych kierunków ruchu (dla rogu
            jeden z trzech). Jeżli warunki ruchu spełnia więcej niż jedno pole to losuje następne
            pole z rozłkadem jednostajnym. 
            */
            
            Point2D myPos = thisCreature.getPosition();
            Creature.CreatureInfo closest = findClosest(Creature.Type.WOLF, SParameters.rabbitRange);

            // If on the edge or no wolves, make a random move
            if (closest == null || onEdge(myPos)) {
                return genMoveType(null, Creature.MoveType.RANDOM);
            }

            // Try to make a perfect move (run away from the wolf)
            Move bestmove = genMoveType(closest, Creature.MoveType.AWAY);

            // Check if the distance is increased (move pos diff > current pos diff)
            if (distance(positionDiff(bestmove.getTo(), closest.creature.getPosition())) > closest.posDiff){
                // That's best move
                return bestmove;
            }

            // Just make random move
            return genMoveType(null, Creature.MoveType.RANDOM);
        }

        private static boolean onEdge(Point2D pos) {
            return (pos.getX() == 0 || pos.getX() == SParameters.nCols - 1)
                || (pos.getY() == 0 || pos.getY() == SParameters.nRows - 1);
        }
    }


    /**
     * Get the default move policy, always runs away from wolves.
     */
    public static MovePolicy getDefault() {
        return new AlwaysRunAwayFromWolves();
    }

    /**
     * Get the move policy as described in the problem statement.
     * It runs away from wolves, and makes random moves if on the edge.
     */
    public static MovePolicy getProblemMovePolicy() {
        return new ProblemMovement();
    }
}
