import java.util.Comparator;
import java.util.LinkedList;
import java.util.PriorityQueue;
import java.util.Vector;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Function;

import javafx.geometry.Point2D;
import javafx.util.Pair;

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
        ALWAYS_TOWRADS_RABBITS
    }

    /**
     * Data class for finding the closest creature
     */
    protected static class CreatureInfo {
        int posDiff = 0;
        Creature creature = null;

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
     * Get array of all policies names (if argument is null, translated using 'getPolicyName')
     */
    public static String[] allPoliciesNames(Function<Policies,String> translate) {
        if (translate == null) {
            translate = new Function<MovePolicy.Policies,String>() {
                @Override
                public String apply(MovePolicy.Policies policy) {
                    return getPolicyName(policy);
                }
            };
        }

        Policies policies[] = Policies.values();
        String[] names = new String[policies.length];
        
        for (int i = 0; i < policies.length; i++) {
            names[i] = translate.apply(policies[i]);
        }

        return names;
    }

    /**
     * Get polish translation of movement policies enum
     */
    public static String getPolicyName(Policies policy) {
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
    
    protected Creature thisCreature = null;
    protected boolean capturePossible = false;

    public void setOwner(Creature owner) {
        thisCreature = owner;
    }

    /**
     * Generate possible moves for this creature (so that it doesn't)
     */
    protected Vector<Move> genPossibleMoves() {
        capturePossible = false;
        Vector<Move> moves = new Vector<>(8);
        Point2D currentPos = thisCreature.getPosition();
        double diffX[] = {-1, 0, 1, -1, 1, -1,  0,  1};
        double diffY[] = { 1, 1, 1,  0, 0, -1, -1, -1};

        for(int i = 0; i < diffX.length; i++) {
            Point2D to = currentPos.add(diffX[i], diffY[i]);

            // Check if the point is within bounds
            if (!Manager.inBounds(to)) {
                continue;
            }

            // at is already synchronized, so no need to synchronize it again
            Creature c = thisCreature.manager.at(to);

            // No occupant at this square
            if (c == null) {
                moves.add(new Move(currentPos, to));
                continue;
            }

            // Occupied by the same type, ignore it
            if (c.getType() == thisCreature.getType()) {
                continue;
            }

            // Check if I can capture this Creature
            if (thisCreature.isCapture(c.getType())) {
                moves.add(new Move(currentPos, to, c));
                capturePossible = true;
            }
        }

        return moves;
    }

    /**
     * Generate a move based on the closest target creature
     * @param info target creature
     * @param type either move towards it, or run away from it
     * @return generated move
     */
    protected Move genMoveType(CreatureInfo info, MoveType type) {

        // Add all possible moves
        Vector<Move> moves = genPossibleMoves();
        Vector<Move> validMoves = new Vector<>(8);
        

        switch (type) {
            case TOWARDS:
                // Simply check if the distance between the target and
                // given move shrinks
                for (Move m : moves) {
                    // If there is a capture, make it the only move
                    if (capturePossible) {
                        if (m.getType() == Move.Type.CAPTURE){
                            validMoves.add(m);
                        }
                    } 
                    else if (distance(positionDiff(m.getTo(), info.creature.getPosition())) < info.posDiff) {
                        validMoves.add(m);
                    }
                }
                break;
            
            case AWAY:
                // If the distance increases, add it to the valid moves
                Vector<Move> keepSameDist = new Vector<>(8);
                for (Move m : moves) {
                    int dist = distance(positionDiff(m.getTo(), info.creature.getPosition()));

                    if (dist > info.posDiff) {
                        validMoves.add(m);
                    }
                    else if (dist == info.posDiff) {
                        keepSameDist.add(m);
                    }
                }

                // Check if ideal moves are not possible,
                // then keep the same distance
                if (validMoves.isEmpty()) {
                    validMoves = keepSameDist;
                }

                break;
            case RANDOM:
            default: 
                // All possible moves are valid
                validMoves = moves;
                break;
        }

        // No valid moves
        if (validMoves.isEmpty()) {
            return null;
        }

        // Only 1, so there is no need to randomly select one
        if (validMoves.size() == 1) {
            return validMoves.firstElement();
        }
        
        // Choose randomly
        return validMoves.get(ThreadLocalRandom.current().nextInt(validMoves.size()));
    }

    /**
     * Get the abs. difference from given point (1st x, 2nd y)
     */
    protected static Pair<Integer, Integer> positionDiff(Point2D p1, Point2D p2) {
        return new Pair<Integer,Integer>(
            Math.abs((int)(p1.getX() - p2.getX())),
            Math.abs((int)(p1.getY() - p2.getY())) 
        );
    }

    /**
     * Get the distance from given point
     */
    protected static int distance(Pair<Integer, Integer> dist) {
        return Math.max(dist.getKey(), dist.getValue());
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
            Comparator<CreatureInfo> cmp = new CreatureInfoComparator();
            PriorityQueue<CreatureInfo> queue = new PriorityQueue<>(thisCreature.creaturesRef.size(), cmp);

            for (Creature c : thisCreature.creaturesRef) {
                if (c.type != type) {
                    continue;
                }

                Pair<Integer, Integer> diffs = positionDiff(c.getPosition(), thisCreature.getPosition());
                int dist = distance(diffs);
                if ((dist == 0) || (dist > maxRange)) {
                    continue;
                }

                // Get the square distance from the target, and put it on the queue
                queue.add(new CreatureInfo(dist, c));
            }

            // No creatures of given type
            if (queue.isEmpty()) {
                return null;
            }

            // Get the closest creature
            CreatureInfo target = queue.poll();
            LinkedList<CreatureInfo> closest = new LinkedList<>();
            closest.add(target);

            // Add other creatures within the distance
            CreatureInfo other;
            while (!queue.isEmpty() && target.posDiff == (other = queue.poll()).posDiff) {
                closest.add(other);
            }

            // Choose at random the target
            int targetIndex = 0;
            if (closest.size() != 1) {
                targetIndex = ThreadLocalRandom.current().nextInt(closest.size());
            }

            // Set the target
            creature = closest.get(targetIndex);
        }

        return creature;
    }
}
