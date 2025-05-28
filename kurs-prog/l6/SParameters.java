
// Class for keeping the simulation parameters
public class SParameters {
    static public double cycleRate = 100;
    static public int nRows = 30;
    static public int nCols = 30;
    static public int wolfCount = 1;
    static public int rabbitCount = 20;
    static public int wolfRange = nRows; // by default max
    static public int rabbitRange = nCols;
    static public int wolfSpeed = 1;
    static public int rabbitSpeed = 1;
    static public MovePolicy.Policies rabbitMovePolicy = MovePolicy.Policies.ALWAYS_RUN_AWAY_FROM_WOLVES;
    static public MovePolicy.Policies wolfMovePolicy = MovePolicy.Policies.ALWAYS_TOWRADS_RABBITS;
}