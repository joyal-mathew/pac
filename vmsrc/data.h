#pragma once

#define AS(type, ptr) *(type *) (ptr)

void debug(char *inst, int len, char *codes, char **v) {
    printf("%s====", inst);
    for (int i = 0; i < len; ++i) {
        printf("\n%c | ", codes[i]);
        for (int j = 0; j < 32; ++j) {
            printf("%hhu ", v[i][j]);
        }
    }
    printf("\n====");
}

char *program;

char *op_stack;
char *var_stack;
char *call_stack;
size_t *alloc_stack;

size_t pc;

size_t sp;
size_t fp;
size_t cp;
size_t ap;
