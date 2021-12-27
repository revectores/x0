#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#ifndef PL0_GEN_X0_H
#define PL0_GEN_X0_H

#define NROW 15
#define TXMAX 100
#define NMAX 14
#define AL 10
#define MAXERR 30
#define MAX_ADDR 2048
#define MAX_LEVEL 3
#define MAX_CX 200
#define STACK_SIZE 500
#define LINE_WIDTH 200

enum symbol {
    nul,       ident,    number,    plus,     minus,
    times,     slash,    odd_sym,   eql,      neq,
    lss,       geq,      gtr,       leq,      lparen,
    rparen,    comma,    semicolon, period,   becomes,
    begin_sym, end_sym,  if_sym,    then_sym, while_sym,
    write_sym, read_sym, do_sym,    call_sym, const_sym,
    var_sym,   func_sym, main_sym,  type_sym, lbracket,
    rbracket,  else_sym, mod,       not_sym,  lor,
    land,      bor,      band,      bxor,     for_sym,
    arrow,     return_sym,
};
#define SYM_CNT 47

enum object {
    constant, variable, function, argument
};

enum type {
    bool_, char_, float_, int_, void_
};
#define NTYPE 5

enum fct {
    lit, opr, lod, sto, cal, ini, jmp, jpc, ldx, stx,
};
#define FCT_CNT 10

struct instruction {
    enum fct f;
    int l;
    int a;
};

enum opcode {
    op_ret,  op_rev,  op_add,  op_sub,  op_mul,
    op_div,  op_odd,           op_eq=8, op_neq,
    op_lt,   op_gte,  op_gt,   op_lte,  op_write,
    op_lf,   op_read, op_mod,  op_cast, op_lor,
    op_land, op_bor,  op_band, op_xor,
};
enum io {
    io_bool, io_char, io_float, io_int
};
enum cast {
    itof, ftoi
};

bool list_switch;
bool table_switch;
char ch;
enum symbol sym;
char id[AL + 1];
int num;
int size;
enum type type;
int cc, ll;
int cx;
int unpaired_begin_cnt;
bool comment;
char line[LINE_WIDTH + 1];
char A[AL + 1];
struct instruction code[MAX_CX];
char word[NROW][AL];
enum symbol wsym[NROW];
enum symbol ssym[256];
char mnemonic[FCT_CNT][5];
bool declbegsys[SYM_CNT];
bool statbegsys[SYM_CNT];
bool factbegsys[SYM_CNT];

struct table_struct {
    char name[AL];
    enum object kind;
    enum type type;
    int val;
    int level;
    int adr;
    int size;
};

struct table_struct table[TXMAX];

FILE* fin;
FILE* ftable;
FILE* fcode;
FILE* fout;
FILE* fresult;
FILE* fstack;
int err;


void error(int n);
void getsym();
void getch();
void init();
void gen(enum fct f, int l, int a);
void interpret(bool step_mode);
void test(bool *s1, bool *s2, int n);
int inset(int e, const bool *s);
int addset(bool *sr, const bool *s1, const bool *s2, int n);
int subset(bool *sr, const bool *s1, const bool *s2, int n);
int mulset(bool *sr, const bool *s1, const bool *s2, int n);
void list_code(int cx0);
void list_all();
int position(char *idt, int tx);
void enter(enum object k, int *ptx, int lev, int *pdx);
enum type upcast(enum type t1, enum type t2);
int base(int l, int *s, int b);

int compile_and_run(const char *fname, const char *path, bool step_mode);
void block(int lev, int tx, bool *fsys);
void statement(bool *fsys, int *ptx, int lev);
enum type expression(bool *fsys, int *ptx, int lev);
enum type var(bool *fsys, int *ptx, int lev);
enum type clause_or(bool *fsys, int *ptx, int lev);
enum type clause_and(bool *fsys, int *ptx, int lev);
enum type bitwise_or(bool *fsys, int *ptx, int lev);
enum type bitwise_xor(bool *fsys, int *ptx, int lev);
enum type bitwise_and(bool *fsys, int *ptx, int lev);
enum type additive_expr(bool *fsys, int *ptx, int lev);
enum type simple_expr(bool *fsys, int *ptx, int lev);
enum type term(bool *fsys, int *ptx, int lev);
enum type unary(bool *fsys, int *ptx, int lev);
enum type factor(bool *fsys, int *ptx, int lev);
void func(bool *fsys, int *ptx, int lev);
void var_decl(int *ptx, int lev, int *pdx);
void const_decl(int *ptx, int lev, int *pdx);
void arg_decl(int *ptx, int lev, int *argc);


#endif
