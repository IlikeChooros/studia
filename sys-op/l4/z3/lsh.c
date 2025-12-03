#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdbool.h>

#define N_HISTORY 10
#define MAX_PATH 1024

bool is_foreground_task(char** args) {
    int i = 0;
    for (; args[i] != NULL; i++) {}
    if (i > 0 && strcmp(args[i - 1], "&") == 0) {
        args[i - 1] = NULL; // Remove '&' from arguments
        return false;
    }
    return true;
}

char** parse_command(const char* command) {
    static char* args[64];
    char* token;
    char* command_copy = strdup(command);
    int index = 0;

    token = strtok(command_copy, " \n");
    while (token != NULL && index < 63) {
        args[index++] = token;
        token = strtok(NULL, " \n");
    }
    args[index] = NULL;
    return args;
}

int exec_cd(char *arg) {
    static char lastdir[MAX_PATH] = "";
    char currentdir[MAX_PATH] = "";

    if (getcwd(currentdir, sizeof(currentdir)) == NULL) {
        perror("getcwd failed");
        return -1;
    }

    if (arg == NULL || strcmp(arg, "~") == 0) {
        arg = getenv("HOME");
    } else if (strcmp(arg, "-") == 0) {
        if (lastdir[0] == '\0') {
            fprintf(stderr, "lsh: cd: lastdir not set\n");
            return -1;
        }
        arg = lastdir;
        printf("%s\n", arg);
    }

    if (chdir(arg) != 0) {
        perror("lsh: cd failed");
        return -1;
    }

    strncpy(lastdir, currentdir, MAX_PATH);
    return 0;
}

int main() {
    // Don't allow Ctrl+C to terminate the shell
    signal(SIGINT, SIG_IGN);

    while (1) {
        printf("lsh> ");
        char command[256];
        if (fgets(command, sizeof(command), stdin) == NULL) {
            break; // EOF
        }

        if (strcmp(command, "exit\n") == 0) {
            break;
        }

        char** args = parse_command(command);
        if (args[0] == NULL) {
            continue; // Empty command
        }

        if (args != NULL && strcmp(args[0], "cd") == 0) {
            exec_cd(args[1]);
            continue;
        }

        // Create a child process to execute the command
        bool should_wait = is_foreground_task(args);
        pid_t pid = fork();
        if (pid == 0) {
            signal(SIGINT, SIG_DFL); // Restore default Ctrl+C behavior in child
            execvp(args[0], args);

            // This will only executed if exec fails
            if (errno == ENOENT) {
                fprintf(stderr, "lsh: command not found: %s\n", args[0]);
            } else {
                fprintf(stderr, "lsh: exec error: %s\n", strerror(errno));
            }
            // perror("lsh: exec failed\n");
            exit(EXIT_FAILURE);
        } else if (pid < 0) {
            perror("lsh: fork failed\n");
            break;
        } else {
            // Wait for all child processes to finish
            if (should_wait) {
                while(waitpid(pid, NULL, 0) > 0);
            } else {
                printf("[%d] %s", pid, command);
            }
        }
    }
}