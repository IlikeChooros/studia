public class PositionDiff {
    private int xdiff;
    private int ydiff;
    private int distance;

    public PositionDiff() {
        xdiff = ydiff = distance = 0;
    }

    public PositionDiff(Position p1, Position p2) {
        set(p1, p2);
    }

    public static int distance(Position p1, Position p2) {
        return Math.max(
            Math.abs(p1.getX() - p2.getX()),
            Math.abs(p1.getY() - p2.getY())
        );
    }

    void set(Position p1, Position p2) {
        xdiff = Math.abs(p1.getX() - p2.getX());
        ydiff = Math.abs(p1.getY() - p2.getY());
        distance = Math.max(xdiff, ydiff);
    }

    int getDiffX() {
        return xdiff;
    }

    int getDiffY() {
        return ydiff;
    }

    int getDistance() {
        return distance;
    }
}
