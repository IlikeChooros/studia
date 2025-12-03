#include <stdio.h>
#include <signal.h>
#include <stdlib.h>


// Signal handler for SIGINT
void every_signal_handler(int signum) {
    printf("Caught signal %d\n", signum);

    if (signum == SIGTERM || signum == SIGINT) {
        printf("Exiting program due to signal %d\n", signum);
        exit(0);
    }
}

int main() {
    // Nie można napisać programu obsługującego wszystkie sygnały, 
    // ponieważ SIGKILL i SIGSTOP nie mogą być nadpisane.
    
    printf("Pid: %d\n", getpid());
    for (int i = 1; i < 64; i++) {
        signal(i, every_signal_handler);
    }

    while (1) {}
    return 0;
}