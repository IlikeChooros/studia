package myapp;

public class Calculator implements Model {
    private double n1 = 0;
    private double n2 = 0;

    public Calculator() {}

    public void set(double n1, double n2) {
        this.n1 = n1;
        this.n2 = n2;
    }

    public double add() {
        return n1 + n2;
    }

    public double sub() {
        return n1 - n2;
    }

    public double mul() {
        return n1 * n2;
    }

    public double div() {
        return n1 / n2;
    }

    public double pow() {
        return Math.pow(n1, n2);
    }
}
