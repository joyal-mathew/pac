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
    alloc_stack = malloc(256);

    char *stacks[] = {
        op_stack,
        var_stack,
        call_stack,
    };

    char skip = 0;
    pc = 24;

    for (;;) {
        int i = program[pc++];
        if (skip) {
            if (i == 15) skip -= 1; // TODO: fix nested call skip bug
            else if (i == 12) skip += 1;
        }
        if (*argv[1] == 'O' && !skip) {
            printf("%d\tsp: %llu\tfp: %llu\tcp: %llu\t pc: %llu\t", i, sp, fp, cp, pc - 1);
            debug(names[i], sizeof(stacks)/sizeof(*stacks), "ovc", stacks);
            if (*argv[3] == 'O') skip = getchar() == 's';
        }
        instructions[i]();
        if (*argv[2] == 'O' && !skip) {
            printf("sp: %llu\tfp:\t%llu\tcp:%llu\t", sp, fp, cp);
            debug(names[i], sizeof(stacks)/sizeof(*stacks), "ovc", stacks);
            if (*argv[3] == 'O') skip = getchar() == 's';
        }

        if (sp > 256) {
            return 1;
        }
    }
}
