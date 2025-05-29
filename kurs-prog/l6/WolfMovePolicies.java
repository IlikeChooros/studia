final public class WolfMovePolicies {

    public static class RunTowardsRabbits extends MovePolicy {

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

    public static class RunTowardsRabbitsWaitAfterKill extends RunTowardsRabbits {

        // Number of cycles to wait, after killing the rabbit
        private int waitCycleAfterKill = 5;

        public RunTowardsRabbitsWaitAfterKill() {}
        public RunTowardsRabbitsWaitAfterKill(int waitCyclesAfterKill) {
            this.waitCycleAfterKill = waitCyclesAfterKill;
        }

        /**
         * Same as `RunTowardsRabbits`, but after killing the rabbit, 
         * waits specified number of cycles
         */
        @Override
        public Move genMove() {
            Move m = super.genMove();

            // If that's a kill, go to sleep
            if (m != null && m.getType().equals(Move.Type.CAPTURE)) {
                thisCreature.suspend(waitCycleAfterKill);
            }

            return m;
        }
    }
};


