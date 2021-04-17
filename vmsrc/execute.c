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

    op_stack = malloc(256);
    var_stack = malloc(256);
    call_stack = malloc(256);
    alloc_stack = malloc(256);

    char *stacks[] = {
        op_stack,
        var_stack,
        call_stack,
    };

    for (;;) {
        int i = program[pc++];
        if (*argv[1] == 'O') {
            printf("%d\tsp: %llu\tfp: %llu\tcp: %llu\t pc: %llu\t", i, sp, fp, cp, pc - 1);
            debug(names[i], sizeof(stacks)/sizeof(*stacks), "ovc", stacks);
            if (*argv[3] == 'O') getchar();
        }
        instructions[i]();
        if (*argv[2] == 'O') {
            printf("sp: %llu\tfp:\t%llu\tcp:%llu\t", sp, fp, cp);
            debug(names[i], sizeof(stacks)/sizeof(*stacks), "ovc", stacks);
            if (*argv[3] == 'O') getchar();
        }
    }
}
