#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <PID>\n", argv[0]);
        return 1;
    }

    int target_pid = atoi(argv[1]);
    for (int i = 0; i < 5; i++) {
        if (kill(target_pid, SIGUSR1) == -1) {
            perror("kill");
            return 1;
        }
        // sleep(1);
        printf("Sent SIGUSR1 %d to process %d\n", i + 1, target_pid);
    }

    printf("Finished\n");
    return 0;
}