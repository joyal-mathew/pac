#include <stdio.h>
#include <string.h>
#include "data.h"

void brk() {
    printf("\n----\n");
    for (int i = 0; i <= 64; i += 8) {
        printf("%lld ", AS(long long, var_stack + i));
    }
    printf("\n----\n");

    // TODO: figure out why can't free op_stack/program here

    exit(0);
}

void err() {
    exit(AS(size_t, op_stack + sp - 8));
}

void print() {
    sp -= 16;
    fwrite((char *) AS(size_t, op_stack + sp), 1, AS(size_t, op_stack + sp + 8), stdout);
}

void flush() {
    fflush(stdout);
}

void alloc() {
    AS(size_t, op_stack + sp - 8) = (size_t) malloc(AS(size_t, op_stack + sp - 8));
}

void realloc_() {
    sp -= 8;
    AS(size_t, op_stack + sp - 8) = (size_t) realloc((void *) AS(size_t, op_stack + sp - 8), AS(size_t, op_stack + sp));
}

void dealloc() {
    sp -= 8;
    free((void *) AS(size_t, op_stack + sp));
}

void memtrans() {
    sp -= 24;
    memcpy((void *) AS(size_t, op_stack + sp), program + AS(size_t, op_stack + sp + 8), AS(size_t, op_stack + sp + 16));
}

void memwrite() {
    sp -= 16;
    memcpy((void *) AS(size_t, op_stack + sp), op_stack + sp - AS(size_t, op_stack + sp + 8), AS(size_t, op_stack + sp + 8));
}

void makestring() {
    long long *arr = (long long *) AS(size_t, op_stack + sp - 8);
    long long len = arr[1] / 8;
    char *str = malloc(len + 16);
    long long *datap = (long long *) arr[3];

    for (long long i = 0; i < len; ++i) {
        str[i + 16] = datap[i];
    }

    AS(long long, str) = 0;
    AS(long long, str + 8) = len;

    AS(size_t, op_stack + sp - 8) = (size_t) str;
}

void (*syscalls[])() = {
    brk, err,
    print, flush,
    alloc, realloc_, dealloc,
    memtrans, memwrite,
    makestring,
};
