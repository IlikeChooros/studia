import javafx.geometry.Point2D;

// Modifiable 2d integer point (a position of the creature)
public class Position {
    private int x;
    private int y;

    Position(int x, int y) {
        set(x, y);
    }

    Position(Point2D point) {
        set((int)point.getX(), (int)point.getY());
    }

    Position add(int dx, int dy) {
        return new Position(this.x + dx, this.y + dy);
    }

    void set(int x, int y) {
        this.x = x;
        this.y = y;
    }

    int getX() {
        return x;
    }

    int getY() {
        return y;
    }
}
