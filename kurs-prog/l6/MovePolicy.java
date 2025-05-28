import java.util.Comparator;
import java.util.LinkedList;
import java.util.PriorityQueue;
import java.util.Vector;
import java.util.concurrent.ThreadLocalRandom;

import javafx.geometry.Point2D;
import javafx.util.Pair;

abstract public class MovePolicy implements Creature.MoveGenerator {

    /**
     * Type of move to make, either move towards 
     * given creature, or run away from it
     */
    static protected enum MoveType {
        TOWARDS, AWAY, RANDOM
    };

    public enum Policies {
        RANDOM, RABBIT_PROBLEM_MOVEMENT, ALWAYS_RUN_AWAY_FROM_WOLVES,
        ALWAYS_TOWRADS_RABBITS
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
    protected Move genMoveType(Creature.CreatureInfo info, MoveType type) {

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
    static protected Pair<Integer, Integer> positionDiff(Point2D p1, Point2D p2) {
        return new Pair<Integer,Integer>(
            Math.abs((int)(p1.getX() - p2.getX())),
            Math.abs((int)(p1.getY() - p2.getY())) 
        );
    }

    /**
     * Get the distance from given point
     */
    static protected int distance(Pair<Integer, Integer> dist) {
        return Math.max(dist.getKey(), dist.getValue());
    }

    /**
     * Get the closest type of creature, if many withing the same range,
     * select at random
     */
    protected Creature.CreatureInfo findClosest(Creature.Type type, int maxRange) {
        Creature.CreatureInfo creature = null;

        // Working in multi-thread enviorment,
        // the creatures may be modified (deleted),
        // by other threads
        synchronized (thisCreature.creaturesRef) {
            Comparator<Creature.CreatureInfo> cmp = new Creature.CreatureInfoComparator();
            PriorityQueue<Creature.CreatureInfo> queue = new PriorityQueue<>(thisCreature.creaturesRef.size(), cmp);

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
                queue.add(new Creature.CreatureInfo(dist, c));
            }

            // No creatures of given type
            if (queue.isEmpty()) {
                return null;
            }

            // Get the closest creature
            Creature.CreatureInfo target = queue.poll();
            LinkedList<Creature.CreatureInfo> closest = new LinkedList<>();
            closest.add(target);

            // Add other creatures within the distance
            Creature.CreatureInfo other;
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
