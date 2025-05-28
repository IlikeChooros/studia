final public class WolfMovePolicies {

    static class RunTowardsRabbits extends MovePolicy {

        /**
         * Generates a move for the wolf, runs towards the closest rabbit,
         * if there are no rabbits, walks randomly
         */
        @Override
        public Move genMove() {
            CreatureInfo closest = findClosest(Creature.Type.RABBIT, SParameters.wolfRange);

            // No target creatures
            if (closest == null) {
                return genMoveType(null, MoveType.RANDOM);
            }

            // Don't allow death of the rabbit (for example by other wolf)
            synchronized(closest.creature) {
                return genMoveType(closest, MoveType.TOWARDS);
            }
        }
    }
};


