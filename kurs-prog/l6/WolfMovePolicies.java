final public class WolfMovePolicies {


    static class RunTowardsRabbits extends MovePolicy {

        /**
         * Generates a move for the wolf, runs towards the closest rabbit,
         * if there are no rabbits, walks randomly
         */
        @Override
        public Move genMove() {
            Creature.CreatureInfo closest = findClosest(Creature.Type.RABBIT, SParameters.wolfRange);

            // No target creatures
            if (closest == null) {
                return genMoveType(null, Creature.MoveType.RANDOM);
            }

            return genMoveType(closest, Creature.MoveType.TOWARDS);
        }
    }

    public static MovePolicy getDefault() {
        return new RunTowardsRabbits();
    }

};


