#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <termios.h>

#define N_HISTORY 10
#define MAX_PATH 1024

static atomic_char background_task_count = ATOMIC_VAR_INIT(0);
static char history[N_HISTORY][256];
static int history_count = 0;

char* get_input_line() {
    static char line[256];
    memset(line, 0, sizeof(line));
    int history_index = history_count - 1;
    
    // Read the input line + catch EOF, arrow keys etc.
    while(1) {
        char ch = getchar();
        if (ch == EOF) {
            return NULL; // EOF detected
        } else if (ch == '\n') {
            line[strlen(line)] = '\0';
            printf("\n");
            break; // End of line
        } else if (ch == 27) { // Escape character
            // Handle arrow keys for history navigation
            getchar(); // Skip the '[' character
            char arrow = getchar();
            if (arrow == 'A') { // Up arrow
                if (history_count > 0) {
                    // Clear current line
                    for (size_t i = 0; i < strlen(line); i++) {
                        printf("\b \b");
                    }
                    // Load last command from history
                    strcpy(line, history[history_index % N_HISTORY]);
                    printf("%s", line);
                    if (history_index > 0) {
                        history_index--;
                    }
                }
            }
            if (arrow == 'B') { // Down arrow
                if (history_count > 0 && history_index < history_count - 1) {
                    history_index++;
                    // Clear current line
                    for (size_t i = 0; i < strlen(line); i++) {
                        printf("\b \b");
                    }
                    // Load next command from history
                    strcpy(line, history[history_index % N_HISTORY]);
                    printf("%s", line);
                } else {
                    // Clear current line
                    for (size_t i = 0; i < strlen(line); i++) {
                        printf("\033[D \033[D");
                    }
                    line[0] = '\0'; // Clear line
                }
            }
        } else if (ch == 127 || ch == 8) { // Backspace
            if (strlen(line) > 0) {
                line[strlen(line) - 1] = '\0';
                printf("\033[D \033[D"); // Move cursor back, print space, move back again
            }
        } else {
            line[strlen(line)] = ch;
            printf("%c", ch); // Echo the character
        }
    }

    // Store in history
    if (history_count < N_HISTORY) {
        strcpy(history[history_count++], line);
    } else {
        for (int i = 1; i < N_HISTORY; i++) {
            strcpy(history[i - 1], history[i]);
        }
        strcpy(history[N_HISTORY - 1], line);
    }

    return line;
}

char** parse_command(const char* command) {
    static char* args[64];
    char* token;
    char* command_copy = strdup(command);
    int index = 0;

    token = strtok(command_copy, " \n");
    while (token != NULL && index < 63) {
        // Check if token is a environment variable
        if (token[0] == '$') {
            char* env_var = getenv(token + 1);
            if (env_var != NULL) {
                args[index++] = env_var;
            } else {
                args[index++] = ""; // If env var not found, use empty string
            }
        } else {
            args[index++] = token;
        }

        token = strtok(NULL, " \n");
    }
    args[index] = NULL;

    // Check if the last argument is '&'
    if (index > 0 && strcmp(args[index - 1], "&") == 0) {
        args[index - 1] = NULL; // Remove '&' from arguments
        atomic_fetch_add(&background_task_count, 1);
    }

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

void zombie_handler(int signo) {
    if (atomic_load(&background_task_count) == 0) {
        return; // Ignore if there are background tasks
    }

    atomic_fetch_sub(&background_task_count, 1);
    while (waitpid(-1, NULL, WNOHANG) > 0);
}

int main() {
    // Don't allow Ctrl+C to terminate the shell
    signal(SIGINT, SIG_IGN);
    signal(SIGCHLD, zombie_handler); // Handle terminated child processes

    // Disable terminal buffering for immediate input processing
    struct termios oldt, newt;
    tcgetattr(STDIN_FILENO, &oldt);
    newt = oldt;
    newt.c_lflag &= ~(ICANON | ECHO); // Disable canonical mode and echo
    tcsetattr(STDIN_FILENO, TCSANOW, &newt);

    while (1) {
        printf("lsh> ");
        char* command = get_input_line();
        if (command == NULL) {
            break; // EOF
        }

        if (strcmp(command, "exit") == 0) {
            break;
        }

        char** args = parse_command(command);
        if (args[0] == NULL) {
            continue; // Empty command
        }


        // Build-in commands
        if (strcmp(args[0], "cd") == 0) {
            exec_cd(args[1]);
            continue;
        }
        if (strcmp(args[0], "history") == 0) {
            int start = history_count > N_HISTORY ? history_count - N_HISTORY : 0;
            for (int i = start; i < history_count; i++) {
                printf("%d: %s\n", i + 1, history[i % N_HISTORY]);
            }
            continue;
        }

        // Create a child process to execute the command
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
            if (atomic_load(&background_task_count) == 0) {
                while(waitpid(pid, NULL, 0) > 0);
            } else {
                printf("[%d] %s\n", pid, command);
            }
        }
    }

    // Restore terminal settings
    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
    return 0;
}