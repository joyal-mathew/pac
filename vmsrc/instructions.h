#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "data.h"
#include "syscalls.h"

void nop() {
}

void push() {
    AS(long long, op_stack + sp) = AS(long long, var_stack + fp + AS(long long, program + pc));
    sp += 8;
    pc += 8;
}

void push_l() {
    AS(long long, op_stack + sp) = AS(long long, program + pc);
    sp += 8;
    pc += 8;
}

void pop() {
    sp -= 8;
    AS(long long, var_stack + fp + AS(long long, program + pc)) = AS(long long, op_stack + sp);
    pc += 8;
}

void pull() {
    AS(long long, var_stack + fp + AS(long long, program + pc)) = AS(long long, op_stack + sp - 8);
    pc += 8;
}

void clean() {
    sp -= 8;
}

void clear() {
    sp -= 8;
    sp -= AS(size_t, op_stack + sp);
}

void copy() {
    AS(long long, op_stack + sp) = AS(long long, op_stack + sp - 8);
    sp += 8;
}

void clone() {
    size_t len = AS(size_t, program + pc);
    memcpy(op_stack + sp, op_stack + sp - len, len);
    sp += len;
    pc += 8;
}

void swap() {
    long long temp = AS(long long, op_stack + sp - 16);
    AS(long long, op_stack + sp - 16) = AS(long long, op_stack + sp - 8);
    AS(long long, op_stack + sp - 8) = temp;
}

void unoffset() {
    fp -= AS(size_t, program + pc);
    pc += 8;
}

void offset() {
    fp += AS(size_t, program + pc);
    pc += 8;
}

void call() {
    AS(size_t, call_stack + cp) = pc + 8;
    pc = AS(size_t, program + pc);
    cp += 8;
}

void call_s() {
    sp -= 8;
    AS(size_t, call_stack + cp) = pc;
    pc = AS(size_t, op_stack + sp);
    cp += 8;
}

void sys() {
    syscalls[AS(unsigned char, program + pc)]();
    pc += 1;
}

void ret() {
    cp -= 8;
    pc = AS(size_t, call_stack + cp);
}

void jmp() {
    pc = AS(size_t, program + pc);
}

void br() {
    sp -= 8;
    size_t b = !AS(size_t, op_stack + sp);
    pc = -!b & AS(size_t, program + pc) | -b & (pc + 8);
}

void addr() {
    sp += 8;
    AS(size_t, op_stack + sp - 8) = (size_t) op_stack + sp - 16;
}

void deref() {
    AS(size_t, op_stack + sp - 8) = AS(size_t, AS(size_t, op_stack + sp - 8));
}

void store() {
    sp -= 8;
    AS(size_t, AS(size_t, op_stack + sp - 8)) = AS(size_t, op_stack + sp);
}

void fti() {
    AS(long long, op_stack + sp - 8) = (long long) AS(double, op_stack + sp - 8);
}

void itf() {
    AS(double, op_stack + sp - 8) = (double) AS(long long, op_stack + sp - 8);
}

void i_add() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) + AS(long long, op_stack + sp - 8);
}

void i_sub() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) - AS(long long, op_stack + sp - 8);
}

void i_mul() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) * AS(long long, op_stack + sp - 8);
}

void i_div() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) / AS(long long, op_stack + sp - 8);
}

void i_rem() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) % AS(long long, op_stack + sp - 8);
}

void i_eq() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) == AS(long long, op_stack + sp - 8);
}

void i_neq() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) != AS(long long, op_stack + sp - 8);
}

void i_lt() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) < AS(long long, op_stack + sp - 8);
}

void i_gt() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) > AS(long long, op_stack + sp - 8);
}

void i_lte() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) <= AS(long long, op_stack + sp - 8);
}

void i_gte() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) >= AS(long long, op_stack + sp - 8);
}

void i_not() {
    AS(long long, op_stack + sp - 8) = !AS(long long, op_stack + sp - 8);
}

void i_neg() {
    AS(long long, op_stack + sp - 8) = -AS(long long, op_stack + sp - 8);
}

void f_add() {
    sp -= 8;
    AS(double, op_stack + sp - 8) = AS(double, op_stack + sp) + AS(double, op_stack + sp - 8);
}

void f_sub() {
    sp -= 8;
    AS(double, op_stack + sp - 8) = AS(double, op_stack + sp) - AS(double, op_stack + sp - 8);
}

void f_mul() {
    sp -= 8;
    AS(double, op_stack + sp - 8) = AS(double, op_stack + sp) * AS(double, op_stack + sp - 8);
}

void f_div() {
    sp -= 8;
    AS(double, op_stack + sp - 8) = AS(double, op_stack + sp) / AS(double, op_stack + sp - 8);
}

void f_rem() {
    sp -= 8;
    AS(double, op_stack + sp - 8) = fmod(AS(double, op_stack + sp), AS(double, op_stack + sp - 8));
}

void f_eq() {
    sp -= 8;
    AS(double, op_stack + sp - 8) = AS(double, op_stack + sp) == AS(double, op_stack + sp - 8);
}

void f_neq() {
    sp -= 8;
    AS(double, op_stack + sp - 8) = AS(double, op_stack + sp) != AS(double, op_stack + sp - 8);
}

void f_lt() {
    sp -= 8;
    AS(double, op_stack + sp - 8) = AS(double, op_stack + sp) < AS(double, op_stack + sp - 8);
}

void f_gt() {
    sp -= 8;
    AS(double, op_stack + sp - 8) = AS(double, op_stack + sp) > AS(double, op_stack + sp - 8);
}

void f_lte() {
    sp -= 8;
    AS(double, op_stack + sp - 8) = AS(double, op_stack + sp) <= AS(double, op_stack + sp - 8);
}

void f_gte() {
    sp -= 8;
    AS(double, op_stack + sp - 8) = AS(double, op_stack + sp) >= AS(double, op_stack + sp - 8);
}

void f_not() {
    AS(double, op_stack + sp - 8) = !AS(double, op_stack + sp - 8);
}

void f_neg() {
    AS(double, op_stack + sp - 8) = -AS(double, op_stack + sp - 8);
}

void shr() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) >> AS(long long, op_stack + sp - 8);
}

void shl() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) << AS(long long, op_stack + sp - 8);
}

void b_not() {
    AS(long long, op_stack + sp - 8) = ~AS(long long, op_stack + sp - 8);
}

void b_and() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) & AS(long long, op_stack + sp - 8);
}

void b_or() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) | AS(long long, op_stack + sp - 8);
}

void b_xor() {
    sp -= 8;
    AS(long long, op_stack + sp - 8) = AS(long long, op_stack + sp) ^ AS(long long, op_stack + sp - 8);
}

void (*instructions[])() = {
    nop,
    push, push_l, pop, pull, clean, clear, copy, clone, swap,
    unoffset, offset, call, call_s, sys, ret,
    jmp, br,
    addr, deref, store,
    fti, itf,
    i_add, i_sub, i_mul, i_div, i_rem, i_eq, i_neq, i_lt, i_gt, i_lte, i_gte, i_not, i_neg,
    f_add, f_sub, f_mul, f_div, f_rem, f_eq, f_neq, f_lt, f_gt, f_lte, f_gte, f_not, f_neg,
    shr, shl,
    b_not,
    b_and, b_or, b_xor,
};

char *names[] = {
    "nop",
    "push", "push_l", "pop", "pull", "clean", "clear", "copy", "clone", "swap",
    "unoffset", "offset", "call", "call_s", "sys", "ret",
    "jmp", "br",
    "addr", "deref", "store",
    "fti", "itf",
    "i_add", "i_sub", "i_mul", "i_div", "i_rem", "i_eq", "i_neq", "i_lt", "i_gt", "i_lte", "i_gte", "i_not", "i_neg",
    "f_add", "f_sub", "f_mul", "f_div", "f_rem", "f_eq", "f_neq", "f_lt", "f_gt", "f_lte", "f_gte", "f_not", "f_neg",
    "shr", "shl",
    "b_not",
    "b_and", "b_or", "b_xor",
};
