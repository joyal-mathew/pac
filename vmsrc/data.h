#pragma once

#define AS(type, ptr) *(type *) (ptr)

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
