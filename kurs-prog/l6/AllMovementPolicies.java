
// Class for getting given type movement policy as an MovePolicy object
public class AllMovementPolicies {

    // Randomly select move, that is non-blocking
    public static class RandomMovePolicy extends MovePolicy {

        /**
         * Generates a random move for the creature, takes into account other creatures,
         * and edges
         */
        @Override
        public Move genMove() {
            return genMoveType(null, MoveType.RANDOM);
        }
    }
    

    /**
     * Get the movepolicy based on the given policy type.
     * @param policy
     * @return new MovePolicy instance based on the policy type
     */
    public static MovePolicy get(MovePolicy.Policies policy) {
        switch (policy) {
            case RANDOM:
                return new RandomMovePolicy();
            case RABBIT_PROBLEM_MOVEMENT:
                return new RabbitMovePolicies.ProblemMovement();
            case ALWAYS_RUN_AWAY_FROM_WOLVES:
                return new RabbitMovePolicies.AlwaysRunAwayFromWolves();
            case ALWAYS_TOWRADS_RABBITS:
                return new WolfMovePolicies.RunTowardsRabbits();
            case WAIT_AFTER_KILL:
                return new WolfMovePolicies.RunTowardsRabbitsWaitAfterKill();
            default:
                throw new IllegalArgumentException("Unknown movement policy: " + policy);
        }
    }
}
