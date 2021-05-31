#include <stdio.h>
#include <stdlib.h>
#include "data.h"
#include "instructions.h"

int main(int argc, char **argv) {
    FILE *fptr;
    size_t len;

    fopen_s(&fptr, "bin/main.pab", "rb");
    fseek(fptr, 0, SEEK_END);
    len = ftell(fptr);
    program = malloc(len);
    rewind(fptr);
    fread(program, 1, len, fptr);
    fclose(fptr);

    op_stack = malloc(AS(size_t, program));
    var_stack = malloc(AS(size_t, program + 8));
    call_stack = malloc(AS(size_t, program + 16));

    pc = 24;

    for (;;) instructions[program[pc++]]();
}
