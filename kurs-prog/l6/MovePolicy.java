import java.util.Comparator;
import java.util.LinkedList;
import java.util.PriorityQueue;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Function;

abstract public class MovePolicy implements Creature.MoveGenerator {

    /**
     * Type of move to make, either move towards 
     * given creature, or run away from it
     */
    protected static enum MoveType {
        TOWARDS, AWAY, RANDOM
    };

    public static enum Policies {
        RANDOM, RABBIT_PROBLEM_MOVEMENT, ALWAYS_RUN_AWAY_FROM_WOLVES,
        ALWAYS_TOWRADS_RABBITS, WAIT_AFTER_KILL
    }

    /**
     * Data class for finding the closest creature
     */
    protected static class CreatureInfo {
        int posDiff = 0;
        Creature creature = null;

        public CreatureInfo() {}

        public CreatureInfo(int diff, Creature c) {
            this.posDiff = diff;
            this.creature = c;
        }
    };

    /**
     * Comparator for PriorityQueue, when looking for closest creature, puts first 
     * the closest ones (ascending order)
     */
    protected static class CreatureInfoComparator implements Comparator<CreatureInfo> {
        @Override
        public int compare(CreatureInfo x, CreatureInfo y) {
            // negative -> x < y (so x should be in front)
            // zero -> x == y (are the same)
            // positive -> x > y (so y should be in front of the queue)
            return x.posDiff - y.posDiff;
        }
    }

    /**
     * Get array of given policies names (if argument is null, translated using 'getPolicyName')
     */
    public static String[] getPoliciesNames(Function<Policies,String> translate, Policies... policies) {
        if (translate == null) {
            translate = new Function<MovePolicy.Policies,String>() {
                @Override
                public String apply(MovePolicy.Policies policy) {
                    return getPolicyNameEN(policy);
                }
            };
        }
        
        String[] names = new String[policies.length];
        
        for (int i = 0; i < policies.length; i++) {
            names[i] = translate.apply(policies[i]);
        }

        return names;
    }

    /**
     * Get polish translation of movement policies enum
     */
    public static String getPolicyNamePL(Policies policy) {
        switch (policy) {
            case RABBIT_PROBLEM_MOVEMENT:
                return "Ruch królika zgodny z zadaniem";
            case ALWAYS_RUN_AWAY_FROM_WOLVES:
                return "Zawsze uciekaj od wilków";
            case ALWAYS_TOWRADS_RABBITS:
                return "Zawsze biegnij w stronę królików";
            default:
            case RANDOM:
                return "Losowe ruchy";
        }
    }

    /**
     * Get polish translation of movement policies enum
     */
    public static String getPolicyNameEN(Policies policy) {
        switch (policy) {
            case RABBIT_PROBLEM_MOVEMENT:
                return "Rabbit's problem movement";
            case ALWAYS_RUN_AWAY_FROM_WOLVES:
                return "Always run away from wolves";
            case ALWAYS_TOWRADS_RABBITS:
                return "Run towards rabbits";
            case WAIT_AFTER_KILL:
                return "After killing the rabbit, short sleep";
            default:
            case RANDOM:
                return "Random move";
        }
    }
    
    protected Creature thisCreature = null;
    protected boolean capturePossible = false;

    // Reduce object creation per cycle
    private final MoveList reusableMoves = new MoveList();
    private final MoveList reusableValidMoves = new MoveList();
    private final MoveList reusableKeepSameDistMoves = new MoveList();
    private final PriorityQueue<CreatureInfo> reusableQueue = new PriorityQueue<>(10, new CreatureInfoComparator());
    private final LinkedList<CreatureInfo> reusableClosestList = new LinkedList<>();


    public void setOwner(Creature owner) {
        thisCreature = owner;
    }

    /**
     * Generate possible moves for this creature,
     * sets generated moves into
     */
    protected void genPossibleMoves() {
        capturePossible = false;
        reusableMoves.clear();
        Position currentPos = thisCreature.getPosition();
        int diffX[] = {-1, 0, 1, -1, 1, -1,  0,  1};
        int diffY[] = { 1, 1, 1,  0, 0, -1, -1, -1};

        for(int i = 0; i < diffX.length; i++) {
            Position to = currentPos.add(diffX[i], diffY[i]);

            // Check if the point is within bounds
            if (!Manager.inBounds(to)) {
                continue;
            }

            // at is already synchronized, so no need to synchronize it again
            Creature c = thisCreature.manager.at(to);

            // No occupant at this square
            if (c == null) {
                reusableMoves.add(currentPos, to, null);
                continue;
            }

            // Occupied by the same type, ignore it
            if (c.getType() == thisCreature.getType()) {
                continue;
            }

            // Check if I can capture this Creature (don't allow suspended creatures)
            if (!c.getSuspended() && thisCreature.isCapture(c.getType())) {
                reusableMoves.add(currentPos, to, c);
                capturePossible = true;
            }
        }
    }

    /**
     * Generate a move based on the closest target creature, this function
     * should be in a `synchronized` block (info.creature as target), to avoid
     * sudden death of the target, when trying the get it's position etc.
     * @param info target creature
     * @param type either move towards it, or run away from it
     * @return generated move
     */
    protected Move genMoveType(CreatureInfo info, MoveType type) {

        // Add all possible moves
        genPossibleMoves();
        reusableValidMoves.clear();
        
        // No target, make a random move
        if (info == null || info.creature == null) {
            type = MoveType.RANDOM;
        }

        switch (type) {
            case TOWARDS:
                // Simply check if the distance between the target and
                // given move shrinks
                for (Move m : reusableMoves) {
                    // If there is a capture, make it the only move
                    if (capturePossible) {
                        if (m.getType() == Move.Type.CAPTURE){
                            reusableValidMoves.add(m);
                        }
                    }
                    else if (PositionDiff.distance(m.getTo(), info.creature.getPosition()) < info.posDiff) {
                        reusableValidMoves.add(m);
                    }
                }
                break;
            
            case AWAY:
                // If the distance increases, add it to the valid moves
                reusableKeepSameDistMoves.clear();
                for (Move m : reusableMoves) {
                    int dist = PositionDiff.distance(m.getTo(), info.creature.getPosition());

                    if (dist > info.posDiff) {
                        reusableValidMoves.add(m);
                    }
                    else if (dist == info.posDiff) {
                        reusableKeepSameDistMoves.add(m);
                    }
                }

                // Check if ideal moves are not possible,
                // then keep the same distance
                if (reusableValidMoves.isEmpty()) {
                    reusableValidMoves.setAll(reusableKeepSameDistMoves);
                }

                break;
            case RANDOM:
            default:
                // All possible moves are valid
                reusableValidMoves.setAll(reusableMoves);
                break;
        }

        // No valid moves
        if (reusableValidMoves.isEmpty()) {
            return null;
        }

        // Only 1, so there is no need to randomly select one
        if (reusableValidMoves.size() == 1) {
            return reusableValidMoves.firstElement();
        }
        
        // Choose randomly
        return reusableValidMoves.get(ThreadLocalRandom.current().nextInt(reusableValidMoves.size()));
    }

    /**
     * Get the closest type of creature, if many withing the same range,
     * select at random
     */
    protected CreatureInfo findClosest(Creature.Type type, int maxRange) {
        CreatureInfo creature = null;

        // Working in multi-thread enviorment,
        // the creatures may be modified (deleted),
        // by other threads
        synchronized (thisCreature.creaturesRef) {
            for (Creature c : thisCreature.creaturesRef) {
                if (c.type != type || c.getSuspended()) {
                    continue;
                }

                int dist = PositionDiff.distance(c.getPosition(), thisCreature.getPosition());
                if ((dist == 0 || thisCreature == c) || (dist > maxRange)) {
                    continue;
                }

                // Get the square distance from the target, and put it on the queue
                reusableQueue.add(new CreatureInfo(dist, c));
            }

            // No creatures of given type
            if (reusableQueue.isEmpty()) {
                reusableQueue.clear();
                return null;
            }

            // Get the closest creature
            reusableClosestList.clear();
            CreatureInfo target = reusableQueue.poll();
            reusableClosestList.add(target);

            // Add other creatures within the distance
            CreatureInfo other;
            while (!reusableQueue.isEmpty() && target.posDiff == (other = reusableQueue.poll()).posDiff) {
                reusableClosestList.add(other);
            }

            // Choose at random the target
            int targetIndex = 0;
            if (reusableClosestList.size() != 1) {
                targetIndex = ThreadLocalRandom.current().nextInt(reusableClosestList.size());
            }

            // Set the target
            creature = reusableClosestList.get(targetIndex);
            reusableQueue.clear();
        }

        return creature;
    }
}
