#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>


// Signal handler for SIGUSR1
void signal_handler(int signum) {
    printf("Received signal %d\n", signum);
    sleep(4);
}

int main() {
    printf("Pid: %d\n", getpid());
    signal(SIGUSR1, signal_handler);

    while (1) {
        pause(); // Wait for signals
    }

    return 0;
}