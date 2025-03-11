

public class divs {

    // Get the biggest divisor of n
    public static int div(int n) {
        int i = 2, d = 1;

        // Find the smallest divisor, then
        // calculate the biggest divisor < n
        while (i * i <= n) {
            if (n % i == 0) {
                d = n / i;
                break;
            }
            i++;
        }

        return d;
    }

    public static void main(String[] args) {

        for (int i = 0; i < args.length; i++) {
            try {
                System.out.println(args[i] + " " + div(Integer.parseInt(args[i])));
            }
            catch (NumberFormatException e) {
                System.out.println(args[i] + " nie jest liczba calkowita");
            }
        }
    }    
}
