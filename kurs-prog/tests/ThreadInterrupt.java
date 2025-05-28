class InterruptExample implements Runnable {

    public void run() {
        System.out.println(Thread.currentThread().getName() + " started.");
        try {
            for (int i = 1; i <= 10; i++) {
                System.out.println("Processing step " + i);
                Thread.sleep(1000); // Usypianie na 1 sekunde
            }
        } catch (InterruptedException e) {
            System.out.println(Thread.currentThread().getName() + " was interrupted.");
        }
        System.out.println(Thread.currentThread().getName() + " finished.");
    }
}

public class ThreadInterrupt {
    public static void main(String[] args) {
        Thread worker = new Thread(new InterruptExample(), "Worker Thread");
        worker.start();

        // Przerwanie watku po 5 sekundach
        try {
            Thread.sleep(5000); // Glowny watek usypiany na 5 sekunds
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            System.out.println("Main thread was interrupted.");
        }

        System.out.println("Main thread interrupting Worker Thread...");
        worker.interrupt(); // Przerwanie watku 
    }
}
