import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class multply {
    public static void main(String[] args) {
        ExecutorService executor = Executors.newFixedThreadPool(5);
        int[] numbers = {1, 2, 3, 4, 5};

        for (int num : numbers) {
            executor.execute(new Worker(num));
        }

        executor.shutdown();
    }

    static class Worker implements Runnable {
        private final int num;

        Worker(int num) {
            this.num = num;
        }

        @Override
        public void run() {
            int result = num * num;
            System.out.printf("The square of %d is %d\n", num, result);
        }
    }
}
