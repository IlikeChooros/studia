
// Class for keeping the simulation parameters
public class SParameters {
    public static double cycleRate = 100;
    public static int nRows = 30;
    public static int nCols = 30;
    public static int wolfCount = 1;
    public static int rabbitCount = 20;
    public static int wolfRange = nRows; // by default max
    public static int rabbitRange = nCols;
    public static int wolfSpeed = 1;
    public static int rabbitSpeed = 1;
    public static MovePolicy.Policies rabbitMovePolicy = MovePolicy.Policies.ALWAYS_RUN_AWAY_FROM_WOLVES;
    public static MovePolicy.Policies wolfMovePolicy = MovePolicy.Policies.ALWAYS_TOWRADS_RABBITS;
}