
public class Test {

    public static void main(String[] args) {

        if (args.length == 0) {
            System.out.println("Brak argumentów");
            return;
        }

        int n = 0;
        try {
            n = Integer.parseInt(args[0]);
        }
        catch (NumberFormatException e) {
            System.out.println(args[0] + " - nieprawidłowa dana");
            return;
        }

        if (n < 0) {
            System.out.println(n + " - Nieprawidłowy numer wiersza");
            return;
        }

        WierszTrojkataPascala w = new WierszTrojkataPascala(n);
        
        for (int i = 1; i < args.length; i++)
        {
            try {
                n = Integer.parseInt(args[i]);
                System.out.println(args[i] + " - " + w.get(n));
            }
            catch (NumberFormatException e) {
                System.out.println(args[i] + " - nieprawidłowa dana");
            }
            catch (OutOfRangeException e) {
                System.out.println(e.getMessage());
            }
        }

    }
}
