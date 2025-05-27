

class RunTowardsRabbits extends MovePolicy {

    public RunTowardsRabbits(Creature creature) {
        super(creature);
    }

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
