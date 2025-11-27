#include <stdio.h>
#include <string.h>

#define COLOR "\033[%dm"
#define RESET "\033[0m"

void print_hello() {
	const char* str = "Hello, world!";
	int len = strlen(str);
	for (int i = 0; i < len; i++) {
		printf(COLOR"%c",(i%8)+31,str[i]);
	}
	printf(RESET"\n");
}

void print_hellos() {
	for (int i = 30; i <= 38; i++){
		if (i == 30) {
			printf(COLOR, 47);
		}
		printf(COLOR"Hello, world!"RESET"\n", i);
	}
}
	
int main(int argc, char** argv) {
	print_hello();
	print_hellos();
	return 0;
}
