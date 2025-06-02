
public class RabbitMovePolicies {
    
    public static class AlwaysRunAwayFromWolves extends MovePolicy {
        /**
         * Generates a move for the rabbit, runs away from the closest wolf,
         * if there is no wolves, walks randomly
         */
        @Override
        public Move genMove() {
            CreatureInfo closest = findClosest(Creature.Type.WOLF, SParameters.rabbitRange);

            // No target creatures
            if (closest == null) {
                return genMoveType(null, MoveType.RANDOM);
            }

            // Dont' allow death of the target
            synchronized (closest.creature) {
                return genMoveType(closest, MoveType.AWAY); 
            }
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

            CreatureInfo closest = findClosest(Creature.Type.WOLF, SParameters.rabbitRange);

            // No target creatures
            if (closest == null || closest.creature == null) {
                return genMoveType(null, MoveType.RANDOM);
            }

            // Lock the target
            synchronized (closest.creature) {

                // If on the edge o no wolves, make a random move
                if (onEdge(thisCreature.getPosition())) {
                    return genMoveType(null, MoveType.RANDOM);
                }

                // Try to make a perfect move (run away from the wolf)
                Move bestmove = genMoveType(closest, MoveType.AWAY);

                // Check if the distance is increased (move pos diff > current pos diff)
                if (bestmove != null && PositionDiff.distance(bestmove.getTo(), closest.creature.getPosition()) > closest.posDiff){
                    // That's best move
                    return bestmove;
                }
            }

            // Stay still
            return null;
        }

        private static boolean onEdge(Position pos) {
            return (pos.getX() == 0 || pos.getX() == SParameters.nCols - 1)
                || (pos.getY() == 0 || pos.getY() == SParameters.nRows - 1);
        }
    }
}
