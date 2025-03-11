// import java.util.Arrays;

class OutOfRangeException extends Exception {
    public OutOfRangeException(String message) {
        super(message);
    }
}

public class WierszTrojkataPascala {

    private int[] pascal;

    WierszTrojkataPascala(int n) {
        init(n);
    }

    public void init(int n) {

        pascal = new int[n + 1];
        pascal[0] = 1;

        for (int i = 1; i <= n; i++) {
            int prev = 1;

            // pascal[j] += pascal[j - 1] won't work
            // because pascal[j] will be overwritten
            for (int j = 1; j <= i; j++) {
                int temp = pascal[j];
                pascal[j] = prev + pascal[j];
                prev = temp;
            }
        }

        // System.out.println(Arrays.toString(pascal));
    }

    public int get(int i) throws OutOfRangeException {
        if (i < 0 || i >= pascal.length)
            throw new OutOfRangeException(i + " - liczba spoza zakresu");
        return pascal[i];
    }
}