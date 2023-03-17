/*
  Run-time support library of MicroC
*/
#include <stdio.h>
#include <stdlib.h>

int getint() {
    char buffer[32];
    if (fgets(buffer, 32, stdin) == NULL)
      return 0;
    return atoi(buffer);
}

void print(int num) {
  printf("%d\n", num);
}

void printchar(char ch) {
  printf("%c", ch);
}

void printbool(int b) {
  if (b) {
    printf("true\n");
  } else {
    printf("false\n");
  }
}