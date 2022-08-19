#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <signal.h>
#include "x0.h"

#define NROW 17
#define TXMAX 100
#define NMAX 14
#define AL 64
#define MAXERR 30
#define MAX_ADDR 2048
#define MAX_LEVEL 3
#define MAX_CX 200
#define STACK_SIZE 500
#define LINE_WIDTH 200
#define NDIMENSION 10

enum symbol {
    nul,       ident,      number_integer,   plus,      minus,
    times,     slash,      odd_sym,   eql,       neq,
    lss,       geq,        gtr,       leq,       lparen,
    rparen,    comma,      semicolon, period,    becomes,
    begin_sym, end_sym,    if_sym,    then_sym,  while_sym,
    write_sym, read_sym,   do_sym,    call_sym,  const_sym,
    var_sym,   func_sym,   main_sym,  type_sym,  lbracket,
    rbracket,  else_sym,   mod,       not_sym,   lor,
    land,      bor,        band,      bxor,      for_sym,
    arrow,     return_sym, true_,     false_,    number_float
};
#define SYM_CNT 50

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
union {
    int i;
    float f;
} num;
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
    void* obj;
};

struct array_obj {
    int dn[NDIMENSION];
    int mags[NDIMENSION];
} *aop;

struct table_struct table[TXMAX];

enum errors {
    ERROR = 0,
    ERROR_SYNTAX = 100,
    ERROR_SYNTAX_EXPECT_MAIN,
    ERROR_SYNTAX_EXPECT_TYPE,
    ERROR_SYNTAX_EXPECT_SEMICOLON,
    ERROR_SYNTAX_EXPECT_BEGIN,
    ERROR_SYNTAX_EXPECT_END,
    ERROR_SYNTAX_EXPECT_IDENTIFIER,
    ERROR_SYNTAX_EXPECT_NUMBER,
    ERROR_SYNTAX_EXPECT_ASSIGNMENT,
    ERROR_SYNTAX_EXPECT_LPAREN,
    ERROR_SYNTAX_EXPECT_RPAREN,
    ERROR_SYNTAX_EXPECT_LBRACKET,
    ERROR_SYNTAX_EXPECT_RBRACKET,
    ERROR_SYNTAX_EXPECT_VARIABLE,
    ERROR_SYNTAX_EXPECT_FUNCTION,
    ERROR_TYPE = 200,
    ERROR_TYPE_EXPECT_INT,
    ERROR_TYPE_EXPECT_BOOL,
    ERROR_TYPE_NOT_ARRAY,
    ERROR_TYPE_IS_ARRAY,
    ERROR_TYPE_UNDEFINED_IDENTIFIER,
    ERROR_OVERFLOW = 300,
    ERROR_OVERFLOW_LEVEL,
    ERROR_OVERFLOW_INT,
    ERROR_OVERFLOW_ADDRESS,
    ERROR_OVERFLOW_DIMENSION,
    ERROR_UNKNOWN = 400,
    ERROR_UNKNOWN_SYMBOL,
};

enum runtime_errors {
    INVALID_INPUT = 1,
    INVALID_OPERAND,
    UNKNOWN_OPERATOR,
};

FILE* fin;
FILE* ftable;
FILE* fcode;
FILE* fout;
FILE* fresult;
FILE* fstack;
int err;


char symbol_words[SYM_CNT][12] = {
    "nul",       "ident",      "number",    "plus",      "minus",
    "times",     "slash",      "odd_sym",   "eql",       "neq",
    "lss",       "geq",        "gtr",       "leq",       "lparen",
    "rparen",    "comma",      "semicolon", "period",    "becomes",
    "begin_sym", "end_sym",    "if_sym",    "then_sym",  "while_sym",
    "write_sym", "read_sym",   "do_sym",    "call_sym",  "const_sym",
    "var_sym",   "func_sym",   "main_sym",  "type_sym",  "lbracket",
    "rbracket",  "else_sym",   "mod",       "not_sym",   "lor",
    "land",      "bor",        "band",      "bxor",      "for_sym",
    "arrow",     "return_sym", "true_",     "false_",    "number_float",
};

char type_word[NTYPE][10] = {
    "bool", "char", "float", "int", "void"
};

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

void dump_set(const bool *s) {
    for (int i = 0; i < SYM_CNT; i++) if (s[i]) printf("%s ", symbol_words[i]);
    printf("\n");
}

void dump_sym() {
    printf("current sym: %s\n", symbol_words[sym]);
}

int compile_and_run(const char *source, const char *path, bool step_mode) {
    char fname[200];
    bool nxtlev[SYM_CNT];

    if (!(fin = fopen(source, "r"))) {
        printf("cannot open the input file\n");
        exit(1);
    }

    ch = fgetc(fin);
    if (ch == EOF) {
        printf("The input file is empty\n");
        fclose(fin);
        exit(1);
    }
    rewind(fin);

    snprintf(fname, sizeof(fname), "%s/%s", path, "fout.txt");
    if (!(fout = fopen(fname, "w"))) {
        printf("cannot open the output file\n");
        exit(1);
    }

    snprintf(fname, sizeof(fname), "%s/%s", path, "ftable.txt");
    if (!(ftable = fopen(fname, "w"))) {
        printf("cannot open ftable.txt file\n");
        exit(1);
    }

    if (step_mode) {
        snprintf(fname, sizeof(fname), "%s/%s", path, "fstack.txt");
        if (!(fstack = fopen(fname, "w"))) {
            printf("cannot open fstack.txt file\n");
            exit(1);
        }
    }

//    printf("list object code?(y/n)");
//    scanf("%s", fname);
//    list_switch = (fname[0] == 'y' || fname[0] == 'Y');
    list_switch = true;

//    printf("list symbol table?(y/n)");
//    scanf("%s", fname);
//    table_switch = (fname[0] == 'y' || fname[0] == 'Y');
    table_switch = true;

    init();
    cc = ll = cx = 0;
    ch = ' ';
    unpaired_begin_cnt = 0;
    err = 0;

    getsym();
    if (sym == main_sym) getsym();
    else error(ERROR_SYNTAX_EXPECT_MAIN);

    addset(nxtlev, declbegsys, statbegsys, SYM_CNT);
    nxtlev[end_sym] = true;

    block(0, 0, nxtlev);

    if (sym != end_sym) error(ERROR_SYNTAX_EXPECT_END);
    if (err == 0) {
        printf("\n===Parsing success!===\n");
        fprintf(fout, "\n===Parsing success!===\n");

        snprintf(fname, sizeof(fname), "%s/%s", path, "fcode.txt");
        if (!(fcode = fopen(fname, "w"))) {
            printf("cannot open fcode.txt\n");
            exit(1);
        }
        snprintf(fname, sizeof(fname), "%s/%s", path, "fresult.txt");
        if (!(fresult = fopen(fname, "w"))) {
            printf("cannot open fresult.txt\n");
            exit(1);
        }

        list_all();
        fclose(fcode);

        interpret(step_mode);
        fclose(fresult);
    } else {
        printf("\n===%d errors in PL/0 program!===\n", err);
        fprintf(fout, "\n===%d errors in PL/0 program!===\n", err);
    }

    fclose(ftable);
    fclose(fout);
    fclose(fin);

    return err;
}



void init(){
    for (int i = 0; i <= 255; i++){
        ssym[i] = nul;
    }
    ssym['+'] = plus;
    ssym['-'] = minus;
    ssym['*'] = times;
    ssym['/'] = slash;
    ssym['%'] = mod;
    ssym['^'] = bxor;
    ssym['('] = lparen;
    ssym[')'] = rparen;
    ssym['='] = becomes;
    ssym[','] = comma;
    ssym['.'] = period;
    ssym[';'] = semicolon;
    ssym['{'] = begin_sym;
    ssym['}'] = end_sym;
    ssym['['] = lbracket;
    ssym[']'] = rbracket;

    strcpy(&(word[0][0]), "call");
    strcpy(&(word[1][0]), "const");
    strcpy(&(word[2][0]), "do");
    strcpy(&(word[3][0]), "else");
    strcpy(&(word[4][0]), "false");
    strcpy(&(word[5][0]), "for");
    strcpy(&(word[6][0]), "func");
    strcpy(&(word[7][0]), "if");
    strcpy(&(word[8][0]), "main");
    strcpy(&(word[9][0]), "odd");
    strcpy(&(word[10][0]), "read");
    strcpy(&(word[11][0]), "return");
    strcpy(&(word[12][0]), "then");
    strcpy(&(word[13][0]), "true");
    strcpy(&(word[14][0]), "var");
    strcpy(&(word[15][0]), "while");
    strcpy(&(word[16][0]), "write");

    wsym[0] = call_sym;
    wsym[1] = const_sym;
    wsym[2] = do_sym;
    wsym[3] = else_sym;
    wsym[4] = false_;
    wsym[5] = for_sym;
    wsym[6] = func_sym;
    wsym[7] = if_sym;
    wsym[8] = main_sym;
    wsym[9] = odd_sym;
    wsym[10] = read_sym;
    wsym[11] = return_sym;
    wsym[12] = then_sym;
    wsym[13] = true_;
    wsym[14] = var_sym;
    wsym[15] = while_sym;
    wsym[16] = write_sym;

    strcpy(&(mnemonic[lit][0]), "lit");
    strcpy(&(mnemonic[opr][0]), "opr");
    strcpy(&(mnemonic[lod][0]), "lod");
    strcpy(&(mnemonic[sto][0]), "sto");
    strcpy(&(mnemonic[cal][0]), "cal");
    strcpy(&(mnemonic[ini][0]), "ini");
    strcpy(&(mnemonic[jmp][0]), "jmp");
    strcpy(&(mnemonic[jpc][0]), "jpc");
    strcpy(&(mnemonic[ldx][0]), "ldx");
    strcpy(&(mnemonic[stx][0]), "stx");

    for (int i = 0; i < SYM_CNT; i++){
        declbegsys[i] = false;
        statbegsys[i] = false;
        factbegsys[i] = false;
    }

    declbegsys[const_sym] = true;
    declbegsys[var_sym]   = true;
    declbegsys[func_sym]  = true;

    statbegsys[begin_sym] = true;
    statbegsys[call_sym]  = true;
    statbegsys[if_sym]    = true;
    statbegsys[while_sym] = true;
    statbegsys[read_sym]  = true;
    statbegsys[write_sym] = true;
    statbegsys[ident]     = true;
    statbegsys[for_sym]   = true;

    factbegsys[ident]  = true;
    factbegsys[number_integer] = true;
    factbegsys[number_float]   = true;
    factbegsys[true_]  = true;
    factbegsys[false_] = true;
    factbegsys[lparen] = true;
}

int inset(int e, const bool *s){
    return s[e];
}

int addset(bool *sr, const bool *s1, const bool *s2, int n){
    for (int i = 0; i < n; i++) sr[i] = s1[i] | s2[i];
    return 0;
}

int subset(bool *sr, const bool *s1, const bool *s2, int n){
    for (int i = 0; i < n; i++) sr[i] = s1[i] && (!s2[i]);
    return 0;
}

int mulset(bool *sr, const bool *s1, const bool *s2, int n){
    for (int i = 0; i < n; i++) sr[i] = s1[i] && s2[i];
    return 0;
}


void error(int n){
    char space[LINE_WIDTH + 1];
    memset(space, 32, LINE_WIDTH + 1);
    space[cc - 1] = 0;
    printf("%s ^ %d\n", space, n);
    fprintf(fout, "%s ^ %d\n", space, n);

    err = err + 1;
    if (err > MAXERR) exit(2);
}


void getch() {
    if (cc == ll) {
        if (feof(fin)) {
            printf("Program incomplete!\n");
            exit(3);
        }
        ll = 0;
        cc = 0;
        printf("%d ", cx);
        fprintf(fout, "%d ", cx);

        ch = ' ';
        while (ch != '\n') {
            if (fscanf(fin, "%c", &ch) == EOF) {
                line[ll] = 0;
                break;
            }

            printf("%c", ch);
            fprintf(fout, "%c", ch);
            line[ll] = ch;
            ll++;
        }
    }

    ch = line[cc];
    cc++;
}


void getsym(){
    int i, j, k;

    while (ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t') getch();
    if (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || (ch == '_')) {
        k = 0;
        do {
            if (k < AL) {
                A[k] = ch;
                k++;
            }
            getch();
        } while (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ('0' <= ch && ch <= '9') || (ch == '_'));
        A[k] = 0;
        strcpy(id, A);
        i = 0;
        j = NROW - 1;
        do {
            k = (i + j) / 2;
            if (strcmp(id, word[k]) <= 0) j = k - 1;
            if (strcmp(id, word[k]) >= 0) i = k + 1;
        } while (i <= j);

        if (i - 1 > j) sym = wsym[k];
        else {
            i = 0;
            j = NTYPE - 1;
            do {
                k = (i + j) / 2;
                if (strcmp(id, type_word[k]) <= 0) j = k - 1;
                if (strcmp(id, type_word[k]) >= 0) i = k + 1;
            } while (i <= j);

            if (i - 1 > j) {
                sym = type_sym;
                type = (enum type)k;
            }
            else sym = ident;
        }
    } else if (ch >= '0' && ch <= '9') {
        k = 0;
        num.i = 0;
        sym = number_integer;
        do {
            num.i = 10 * num.i + ch - '0';
            k++;
            getch();
        } while (ch >= '0' && ch <= '9');
        if (ch == '.') {
            sym = number_float;
            num.f = (float)num.i;
            int p = 10;
            getch();
            do {
                num.f += (float)(ch - '0') / (float)p;
                p *= 10;
                getch();
            } while (ch >= '0' && ch <= '9');
        }
        k--;
        if (k > NMAX) error(ERROR_OVERFLOW_INT);
    } else if (ch == '=') {
        getch();
        if (ch == '=') {
            sym = eql;
            getch();
        } else {
            sym = becomes;
        }
    } else if (ch == '!') {
        getch();
        if (ch == '=') {
            sym = neq;
            getch();
        } else {
            sym = not_sym;
        }
    } else if (ch == '<') {
        getch();
        if (ch == '=') {
            sym = leq;
            getch();
        } else sym = lss;
    } else if (ch == '>') {
        getch();
        if (ch == '=') {
            sym = geq;
            getch();
        } else sym = gtr;
    } else if (ch == '|') {
        getch();
        if (ch == '|') {
            sym = lor;
            getch();
        } else sym = bor;
    } else if (ch == '&') {
        getch();
        if (ch == '&') {
            sym = land;
            getch();
        } else sym = band;
    } else if (ch == '-') {
        getch();
        if (ch == '>') {
            sym = arrow;
            getch();
        } else sym = minus;
    } else if (ch == '/') {
        getch();
        if (ch == '*') {
            getch();
            while (1) {
                while (ch != '*') getch();
                getch();
                if (ch == '/') break;
            }
            getch();
            getsym();
        } else {
            sym = slash;
        }
    } else {
        sym = ssym[ch];
        if (sym == begin_sym) unpaired_begin_cnt++;
        if (sym == end_sym) unpaired_begin_cnt--;
        if (unpaired_begin_cnt > 0) getch();
    }
}


void gen(enum fct f, int l, int a) {
    if (cx >= MAX_CX) {
        printf("program too long\n");
        exit(4);
    }

//    if (a >= MAX_ADDR) {
//        printf("address overflow\n");
//        exit(5);
//    }
    code[cx].f = f;
    code[cx].l = l;
    code[cx].a = a;
    cx++;
}


void test(bool *s1, bool *s2, int n){
    if (!inset(sym, s1)) {
        error(n);
        while (!inset(sym, s1) && !inset(sym, s2)) getsym();
    }
}

void block(int lev, int tx, bool *fsys){
    int dx = 3;
    int tx0 = tx;
    int cx0;
    int argc = 0;
    bool nxtlev[SYM_CNT];

    table[tx].adr = cx;
    gen(jmp, 0, 0);
    if (lev > MAX_LEVEL) error(ERROR_OVERFLOW_LEVEL);

    table[tx0].type = void_;
    if (sym == lparen) {
        do {
            getsym();
            if (sym != type_sym) error(ERROR_SYNTAX_EXPECT_TYPE);
            getsym();
            arg_decl(&tx, lev, &argc);
            argc += 1;
        } while (sym == comma);
        for (int c = 0; c < argc; c++) table[tx - c].adr -= argc + 1;
        if (sym == rparen) getsym(); else error(ERROR_SYNTAX_EXPECT_RPAREN);
    }
    if (sym == arrow) {
        getsym();
        if (sym == type_sym) table[tx0].type = type;
        else error(ERROR_SYNTAX_EXPECT_TYPE);
        getsym();
    }
    if (sym == begin_sym) getsym(); else error(ERROR_SYNTAX_EXPECT_BEGIN);

    do {
        while (sym == const_sym) {
            getsym();
            if (sym == type_sym) getsym();
            else error(ERROR_SYNTAX_EXPECT_TYPE);
            const_decl(&tx, lev, &dx);
            if (sym == semicolon) getsym();
            else error(ERROR_SYNTAX_EXPECT_SEMICOLON);
        }

        while (sym == type_sym) {
            getsym();
            var_decl(&tx, lev, &dx);
            if (sym == semicolon) getsym();
            else error(ERROR_SYNTAX_EXPECT_SEMICOLON);
        }

        while (sym == func_sym) {
            getsym();
            if (sym == ident) {
                enter(function, &tx, lev, &dx);
                getsym();
            } else error(ERROR_SYNTAX_EXPECT_IDENTIFIER);

            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            block(lev + 1, tx, nxtlev);

            if (sym == end_sym) {
                getsym();
                memcpy(nxtlev, statbegsys, sizeof(bool[SYM_CNT]));
                nxtlev[ident] = true;
                nxtlev[func_sym] = true;
                test(nxtlev, fsys, ERROR_SYNTAX);
            } else error(ERROR_SYNTAX_EXPECT_END);
        }

        memcpy(nxtlev, statbegsys, sizeof(bool[SYM_CNT]));
        nxtlev[ident] = true;
        nxtlev[end_sym] = true;
        nxtlev[return_sym] = true;
        test(nxtlev, declbegsys, ERROR_SYNTAX);
    } while (inset(sym, declbegsys));

    code[table[tx0].adr].a = cx;
    table[tx0].adr = cx;
    table[tx0].size = dx;
    cx0 = cx;
    gen(ini, 0, dx);

    if (table_switch) {
        for (int i = 1; i <= tx; i++) {
            switch (table[i].kind) {
                case constant:
                    printf("    %d const %s ", i, table[i].name);
                    printf("val = %d, type = %s, size = %d\n", table[i].val, type_word[table[i].type], table[i].size);
                    fprintf(ftable, "    %d const %s ", i, table[i].name);
                    fprintf(ftable, "val = %d, type = %s, size = %d\n", table[i].val, type_word[table[i].type], table[i].size);
                    break;
                case variable:
                    printf("    %d var %s ", i, table[i].name);
                    printf("lev = %d, addr = %d, type = %s, size = %d\n", table[i].level, table[i].adr, type_word[table[i].type], table[i].size);
                    fprintf(ftable, "    %d var %s ", i, table[i].name);
                    fprintf(ftable, "lev = %d, addr = %d, type = %s, size = %d\n", table[i].level, table[i].adr, type_word[table[i].type], table[i].size);
                    break;
                case function:
                    printf("    %d func %s ", i, table[i].name);
                    printf("lev = %d, addr = %d, size = %d\n", table[i].level, table[i].adr, table[i].size);
                    fprintf(ftable, "    %d func %s ", i, table[i].name);
                    fprintf(ftable, "lev = %d, addr = %d, size = %d\n", table[i].level, table[i].adr, table[i].size);
                    break;
            }
            printf("\n");
            fprintf(ftable, "\n");
        }
    }

    memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
    nxtlev[semicolon]  = true;
    nxtlev[end_sym]    = true;
    nxtlev[return_sym] = true;
    statement(nxtlev, &tx, lev);

    while (inset(sym, statbegsys)) {
        statement(nxtlev, &tx, lev);
    }
    if (sym == return_sym) {
        getsym();
        enum type t = clause_or(nxtlev, &tx, lev);
        if (t != float_ && table[tx0].type == float_) gen(opr, itof, op_cast);
        if (t == float_ && table[tx0].type != float_) gen(opr, ftoi, op_cast);
        gen(sto, 0, 0 - 1 - argc - 1);
        getsym();
    }
    gen(opr, 0, 0);
    memset(nxtlev, 0, sizeof(bool[SYM_CNT]));
    test(fsys, nxtlev, 8);
    list_code(cx0);
}


void enter(enum object k, int *ptx, int lev, int *pdx) {
    (*ptx)++;
    strcpy(table[*ptx].name, id);
    table[*ptx].kind = k;
    switch (k) {
        case constant:
            if (num.i > MAX_ADDR) {
                error(ERROR_OVERFLOW_ADDRESS);
                num.i = 0;
            }
            table[*ptx].val = num.i;
            table[*ptx].type = type;
            table[*ptx].size = 0;
            break;
        case variable:
            table[*ptx].level = lev;
            table[*ptx].adr = *pdx;
            table[*ptx].type = type;
            table[*ptx].size = size;
            *pdx += size ? size : 1;
            if (size) table[*ptx].obj = (void*)aop;
            break;
        case function:
            table[*ptx].level = lev;
            break;
        case argument:
            table[*ptx].level = lev;
            table[*ptx].adr = *pdx;
            table[*ptx].type = type;
            table[*ptx].size = 0;
            break;
    }
}

int position(char *id, int tx) {
    int i;
    strcpy(table[0].name, id);
    i = tx;
    while (strcmp(table[i].name, id) != 0) i--;
    return i;
}

enum type upcast(enum type t1, enum type t2) {
    enum type levels[NTYPE - 1] = {bool_, char_, int_, float_};
    int n = NTYPE - 1;
    while (n--) if (t1 == levels[n] || t2 == levels[n]) return levels[n];
    return bool_;
}

void const_decl(int *ptx, int lev, int *pdx) {
    if (sym == ident) {
        getsym();
        if (sym == becomes) {
            getsym();
            if (sym == number_integer) {
                enter(constant, ptx, lev, pdx);
                getsym();
            } else {
                error(ERROR_SYNTAX_EXPECT_NUMBER);
            }
        } else {
            error(ERROR_SYNTAX_EXPECT_ASSIGNMENT);
        }
    } else {
        error(ERROR_SYNTAX_EXPECT_IDENTIFIER);
    }
}

void var_decl(int *ptx, int lev, int *pdx) {
    if (sym == ident) {
        getsym();
        size = 0;
        aop = (struct array_obj*)calloc(1, sizeof(struct array_obj));
        int d = 0;
        while (sym == lbracket) {
            getsym();
            if (sym == number_integer) {
                aop->dn[d++] = num.i;
                if (!size) size = num.i; else size *= num.i;
                getsym();
                if (sym == rbracket) {
                    getsym();
                } else error(ERROR_SYNTAX_EXPECT_RBRACKET);
            } else error(ERROR_SYNTAX_EXPECT_NUMBER);
            if (d >= NDIMENSION - 1) error(ERROR_OVERFLOW_DIMENSION);
        }
        if (size) {
            d -= 1;
            aop->mags[d] = 1;
            while (d--) aop->mags[d] = aop->mags[d + 1] * aop->dn[d + 1];
        }
        enter(variable, ptx, lev, pdx);

    } else {
        error(ERROR_SYNTAX_EXPECT_IDENTIFIER);
    }
}

void arg_decl(int *ptx, int lev, int *argc) {
    if (sym == ident) {
        getsym();
        enter(argument, ptx, lev, argc);
    } else error(ERROR_SYNTAX_EXPECT_IDENTIFIER);
}

void list_code(int cx0) {
    if (list_switch) {
        printf("\n");
        for (int i = cx0; i < cx; i++) {
            printf("%d %s %d %d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
        }
    }
}

void list_all() {
    if (list_switch) {
        for (int i = 0; i < cx; i++) {
            printf("%d %s %d %d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
            fprintf(fcode, "%d %s %d %d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
        }
    }
}

void statement(bool *fsys, int *ptx, int lev) {
    int i, cx1, cx2, cx3, cx4, cx5;
    bool nxtlev[SYM_CNT];
    enum type t;

    switch(sym) {
        case ident:
            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            expression(nxtlev, ptx, lev);
            if (sym == semicolon) getsym();
            else error(ERROR_SYNTAX_EXPECT_SEMICOLON);
            break;
        case read_sym:
            getsym();
            if (sym == ident) i = position(id, *ptx);
            else i = 0;
            if (i == 0) error(ERROR_TYPE_UNDEFINED_IDENTIFIER);
            else {
                gen(opr, table[i].type, op_read);
                gen(sto, lev - table[i].level, table[i].adr);
            }
            getsym();
            if (sym == semicolon) getsym();
            else error(ERROR_SYNTAX_EXPECT_SEMICOLON);
            break;
        case write_sym:
            getsym();
            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            t = clause_or(nxtlev, ptx, lev);
            gen(opr, t, op_write);
            gen(opr, 0, op_lf);
            if (sym == semicolon) getsym();
            else error(ERROR_SYNTAX_EXPECT_SEMICOLON);
            break;
        case call_sym:
            getsym();
            if (sym != ident) error(ERROR_SYNTAX_EXPECT_IDENTIFIER);
            else {
                i = position(id, *ptx);

                if (i == 0) error(ERROR_TYPE_UNDEFINED_IDENTIFIER);
                else {
                    if (table[i].kind == function) {
                        memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
                        // gen(lit, 0, 0);
                        func(nxtlev, ptx, lev);
                        gen(cal, lev - table[i].level, table[i].adr);
                    }
                    else error(ERROR_TYPE_UNDEFINED_IDENTIFIER);
                }
                if (sym == semicolon) getsym();
                else error(ERROR_SYNTAX_EXPECT_SEMICOLON);
            }
            break;
        case if_sym:
            getsym();
            if (sym == lparen) getsym();
            else error(ERROR_SYNTAX_EXPECT_LPAREN);

            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            nxtlev[rparen] = true;
            clause_or(nxtlev, ptx, lev);

            if (sym == rparen) getsym(); else error(ERROR_SYNTAX_EXPECT_RPAREN);
            cx1 = cx;
            gen(jpc, 0, 0);

            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            nxtlev[else_sym] = true;
            statement(nxtlev, ptx, lev);

            if (sym == else_sym) {
                cx2 = cx;
                gen(jmp, 0, 0);
                getsym();
                code[cx1].a = cx;
                statement(fsys, ptx, lev);
                code[cx2].a = cx;
            } else {
                code[cx1].a = cx;
            }
            break;
        case begin_sym:
            getsym();
            memcpy(nxtlev, fsys, sizeof(bool) * SYM_CNT);
            nxtlev[semicolon] = true;
            nxtlev[end_sym] = true;
            statement(nxtlev, ptx, lev);

            while (inset(sym, statbegsys)) {
                statement(nxtlev, ptx, lev);
            }
            if (sym == end_sym) getsym();
            else error(ERROR_SYNTAX_EXPECT_END);
            break;
        case while_sym:
            cx1 = cx;
            getsym();
            if (sym == lparen) getsym();
            else error(ERROR_SYNTAX_EXPECT_LPAREN);

            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            nxtlev[rparen] = true;
            clause_or(nxtlev, ptx, lev);
            if (sym == rparen) getsym(); else error(ERROR_SYNTAX_EXPECT_RPAREN);
            cx2 = cx;
            gen(jpc, 0, 0);

            statement(fsys, ptx, lev);
            gen(jmp, 0, cx1);
            code[cx2].a = cx;
            break;
        case for_sym:
            getsym();
            if (sym == lparen) getsym();
            else error(ERROR_SYNTAX_EXPECT_LPAREN);

            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            nxtlev[semicolon] = true;
            expression(nxtlev, ptx, lev);
            cx1 = cx;
            if (sym == semicolon) getsym(); else error(ERROR_SYNTAX_EXPECT_SEMICOLON);
            clause_or(nxtlev, ptx, lev);
            if (sym == semicolon) getsym(); else error(ERROR_SYNTAX_EXPECT_SEMICOLON);
            cx2 = cx;
            gen(jpc, 0, 0);
            cx3 = cx;
            gen(jmp, 0, 0);
            nxtlev[semicolon] = false;
            nxtlev[rparen] = true;
            cx4 = cx;
            expression(nxtlev, ptx, lev);
            gen(jmp, 0, cx1);
            if (sym == rparen) getsym(); else error(ERROR_SYNTAX_EXPECT_RPAREN);

            code[cx3].a = cx;
            statement(fsys, ptx, lev);
            gen(jmp, 0, cx4);
            code[cx2].a = cx;
            break;
        default:
            break;
    }

    memset(nxtlev, 0, sizeof(bool[SYM_CNT]));
    test(fsys, nxtlev, 19);
}

enum type expression(bool* fsys, int *ptx, int lev) {
    enum type this_type = int_;
    bool nxtlev[SYM_CNT];
    int i = position(id, *ptx);

    if (i == 0) error(ERROR_TYPE_UNDEFINED_IDENTIFIER);
    else {
        if (table[i].kind != variable && table[i].kind != argument) {
            error(ERROR_SYNTAX_EXPECT_VARIABLE);
            i = 0;
        } else {
            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            nxtlev[becomes] = true;
            this_type = var(nxtlev, ptx, lev);

            if (sym == becomes) {
                getsym();
                memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
                enum type t = clause_or(nxtlev, ptx, lev);
                if (this_type == float_ && t != float_) gen(opr, itof, op_cast);
                if (this_type != float_ && t == float_) gen(opr, ftoi, op_cast);
                gen(table[i].size ? stx : sto, lev - table[i].level, table[i].adr);
            } else error(ERROR_SYNTAX_EXPECT_ASSIGNMENT);
        }
    }
    return this_type;
}

enum type var(bool *fsys, int *ptx, int lev) {
    bool nxtlev[SYM_CNT];
    int i = position(id, *ptx);
    enum type this_type = table[i].type;
    getsym();
    if (table[i].size && sym != lbracket) error(ERROR_TYPE_IS_ARRAY);
    if (!table[i].size && sym == lbracket) error(ERROR_TYPE_NOT_ARRAY);
    int d = 0;
    while (sym == lbracket) {
        getsym();
        memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
        nxtlev[rparen] = true;
        nxtlev[rbracket] = true;
        enum type t = clause_or(nxtlev, ptx, lev);
        if (t == float_) error(ERROR_TYPE_EXPECT_INT);
        // TODO: Implement bounds checking here.
        //  Throw a runtime error if the bounds checking failed.
        if (sym == rbracket) getsym();
        else error(ERROR_SYNTAX_EXPECT_RBRACKET);
        gen(lit, 0, ((struct array_obj*)table[i].obj)->mags[d]);
        gen(opr, 0, op_mul);
        if (d != 0) gen(opr, 0, op_add);
        d++;
    }
    return this_type;
}

enum type clause_or(bool *fsys, int *ptx, int lev) {
    enum type this_type;
    bool nxtlev[SYM_CNT];
    memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
    nxtlev[lor] = true;
    this_type = clause_and(nxtlev, ptx, lev);
    while (sym == lor) {
        this_type = bool_;
        getsym();
        clause_and(nxtlev, ptx, lev);
        gen(opr, 0, op_lor);
    }
    return this_type;
}

enum type clause_and(bool *fsys, int *ptx, int lev) {
    enum type this_type;
    bool nxtlev[SYM_CNT];
    memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
    nxtlev[land] = true;
    this_type = bitwise_or(nxtlev, ptx, lev);
    while (sym == land) {
        this_type = bool_;
        getsym();
        bitwise_or(nxtlev, ptx, lev);
        gen(opr, 0, op_land);
    }
    return this_type;
}

enum type bitwise_or(bool *fsys, int *ptx, int lev){
    enum type this_type;
    bool nxtlev[SYM_CNT];
    memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
    nxtlev[bor] = true;
    this_type = bitwise_xor(nxtlev, ptx, lev);
    while (sym == bor) {
        if (this_type == float_) error(ERROR_TYPE_EXPECT_INT);
        getsym();
        enum type t = bitwise_xor(nxtlev, ptx, lev);
        this_type = upcast(this_type, t);
        gen(opr, 0, op_bor);
    }
    return this_type;
}

enum type bitwise_xor(bool *fsys, int *ptx, int lev){
    enum type this_type;
    bool nxtlev[SYM_CNT];
    memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
    nxtlev[bxor] = true;
    this_type = bitwise_and(nxtlev, ptx, lev);
    while (sym == bxor) {
        if (this_type == float_) error(ERROR_TYPE_EXPECT_INT);
        getsym();
        enum type t = bitwise_and(nxtlev, ptx, lev);
        this_type = upcast(this_type, t);
        gen(opr, 0, op_xor);
    }
    return this_type;
}

enum type bitwise_and(bool *fsys, int *ptx, int lev){
    enum type this_type;
    bool nxtlev[SYM_CNT];
    memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
    nxtlev[band] = true;
    this_type = simple_expr(nxtlev, ptx, lev);
    while (sym == band) {
        if (this_type == float_) error(ERROR_TYPE_EXPECT_INT);
        getsym();
        enum type t = simple_expr(nxtlev, ptx, lev);
        this_type = upcast(this_type, t);
        gen(opr, 0, op_band);
    }
    return this_type;
}

enum type simple_expr(bool *fsys, int *ptx, int lev){
    int type_mask = 0;
    enum type this_type;
    enum symbol relop;
    bool nxtlev[SYM_CNT];
    memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
    nxtlev[eql] = true;
    nxtlev[neq] = true;
    nxtlev[lss] = true;
    nxtlev[leq] = true;
    nxtlev[gtr] = true;
    nxtlev[geq] = true;
    this_type = additive_expr(nxtlev, ptx, lev);
    if (sym == eql || sym == neq || sym == lss || sym == leq || sym == gtr || sym == geq) {
        relop = sym;
        getsym();
        type_mask |= (this_type == float_ ? 0x1 : 0x0);
        enum type t = additive_expr(fsys, ptx, lev);
        type_mask |= (t == float_ ? 0x2 : 0x0);
        gen(opr, type_mask, relop);
        return bool_;
    }
    return this_type;
}

enum type additive_expr(bool *fsys, int *ptx, int lev) {
    enum type this_type;
    enum symbol addop;
    bool nxtlev[SYM_CNT];
    int type_mask = 0;

    if (sym == plus || sym == minus) {
        addop = sym;
        getsym();
        memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
        nxtlev[plus] = true;
        nxtlev[minus] = true;
        this_type = term(nxtlev, ptx, lev);
        if (addop == minus) gen(opr, this_type == float_, 1);
    } else {
        memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
        nxtlev[plus] = true;
        nxtlev[minus] = true;
        this_type = term(nxtlev, ptx, lev);
    }
    while (sym == plus || sym == minus) {
        type_mask |= (this_type == float_ ? 0x1 : 0x0);
        addop = sym;
        getsym();
        memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
        nxtlev[plus] = true;
        nxtlev[minus] = true;
        enum type t = term(nxtlev, ptx, lev);
        type_mask |= (this_type == float_ ? 0x2 : 0x0);
        this_type = upcast(this_type, t);
        gen(opr, type_mask, 2 + (addop == minus));
    }
    return this_type;
}

enum type term(bool *fsys, int *ptx, int lev) {
    enum type this_type;
    enum symbol mulop;
    bool nxtlev[SYM_CNT];
    int type_mask = 0;

    memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
    nxtlev[times] = true;
    nxtlev[slash] = true;
    nxtlev[mod]   = true;
    this_type = unary(nxtlev, ptx, lev);

    while (sym == times || sym == slash || sym == mod) {
        type_mask |= (this_type == float_ ? 0x1 : 0x0);
        mulop = sym;
        getsym();
        enum type t = unary(nxtlev, ptx, lev);
        type_mask |= (t == float_ ? 0x2 : 0x0);
        this_type = upcast(this_type, t);
        printf("type_mask = %d\n", type_mask);
        switch (mulop) {
            case times:
                gen(opr, type_mask, op_mul);
                break;
            case slash:
                gen(opr, type_mask, op_div);
                break;
            case mod:
                gen(opr, type_mask, op_mod);
                break;
            default:
                error(ERROR_UNKNOWN_SYMBOL);
                break;
        }
    }
    return this_type;
}

enum type unary(bool *fsys, int *ptx, int lev) {
    enum type this_type;
    bool nxtlev[SYM_CNT];

    switch (sym) {
        case ident:
        case number_integer:
        case number_float:
        case lparen:
        case true_:
        case false_:
            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            this_type = factor(nxtlev, ptx, lev);
            break;
        case not_sym:
            getsym();
            gen(lit, 0, 1);
            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            enum type t = factor(nxtlev, ptx, lev);
            if (t != bool_) error(ERROR_TYPE_EXPECT_BOOL);
            this_type = t;
            gen(opr, 0, op_sub);
            break;
        case odd_sym:
            getsym();
            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            factor(nxtlev, ptx, lev);
            gen(opr, 0, op_odd);
            this_type = bool_;
            break;
        default:
            error(ERROR_UNKNOWN_SYMBOL);
            break;
    }
    return this_type;
}

enum type factor(bool *fsys, int *ptx, int lev) {
    enum type this_type;
    int i;
    bool nxtlev[SYM_CNT];
    test(factbegsys, fsys, 24);

    while(inset(sym, factbegsys)){
        switch (sym) {
            case ident:
                i = position(id, *ptx);
                if (i == 0) error(ERROR_TYPE_UNDEFINED_IDENTIFIER);
                else {
                    switch (table[i].kind) {
                        case constant:
                            gen(lit, 0, table[i].val);
                            this_type = table[i].type;
                            getsym();
                            break;
                        case variable:
                        case argument:
                            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
                            this_type = var(nxtlev, ptx, lev);
                            gen(table[i].size ? ldx : lod, lev - table[i].level, table[i].adr);
                            break;
                        case function:
                            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
                            gen(lit, 0, 0);
                            func(nxtlev, ptx, lev);
                            this_type = table[i].type;
                            printf("this_type = %d\n", this_type);
                            gen(cal, lev - table[i].level, table[i].adr);
                            break;
                    }
                }
                break;
            case number_integer:
                if (num.i > MAX_ADDR) {
                    error(ERROR_OVERFLOW_ADDRESS);
                    num.i = 0;
                }
                gen(lit, 0, num.i);
                this_type = int_;
                getsym();
                break;
            case number_float:
                gen(lit, 1, num.i);
                this_type = float_;
                getsym();
                break;
            case true_:
            case false_:
                gen(lit, 0, sym == true_ ? 1 : 0);
                this_type = bool_;
                getsym();
                break;
            case lparen:
                getsym();
                memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
                nxtlev[rparen] = true;
                this_type = clause_or(nxtlev, ptx, lev);
                if (sym == rparen) getsym();
                else error(ERROR_SYNTAX_EXPECT_RPAREN);
                break;
            default:
                break;
        }

        memset(nxtlev, 0, sizeof(bool[SYM_CNT]));
        nxtlev[lparen] = true;
        test(fsys, nxtlev, ERROR_SYNTAX);
    }
    return this_type;
}

void func(bool *fsys, int *ptx, int lev) {
    bool nxtlev[SYM_CNT];

    int argc = 0;
    getsym();
    if (sym == lparen) {
        do {
            getsym();
            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            nxtlev[comma] = true;
            nxtlev[rparen] = true;
            clause_or(nxtlev, ptx, lev);
            argc += 1;
        } while (sym == comma);
        if (sym == rparen) getsym();
        else error(ERROR_SYNTAX_EXPECT_RPAREN);
    }
    gen(lit, 0, argc + 1);
}

void dump_stack(int *s, int t) {
    printf("t = %d\n", t);
    fflush(NULL);
    for (int c = 0; c <= t; c++) printf("%d\n", s[c]);
    printf("\n");
}

void runtime_error(int error_code) {
    printf("runtime error: %d\n", error_code);
    exit(6);
}

void dump_stacks_to_files(const void *sp, const enum type *st, int c, int p, int t) {
    char fname[80];
    sprintf(fname, "fstack/c%d-p%d", c, p);
    printf("%s\n", fname);
    fflush(NULL);
    FILE* fp = fopen(fname, "w");
    if (!fp) exit(20);
    for (int x = 0; x <= t; x++) {
        printf("%d\n", x);
        switch (st[x]) {
            case int_:
            case bool_:
            case char_:
                fprintf(fp, "%d\n", *((int*)sp + x));
                break;
            case float_:
                fprintf(fp, "%f\n", *((float*)sp + x));
                break;
            case void_:
                break;
        }
    }
    fflush(NULL);
    fclose(fp);
}

void interpret(bool step_mode) {
    int p = 0;
    int b = 1;
    int t = 0;
    int cnt = 0;
    char buffer[6];
    struct instruction i;
    union {
        void*  p;
        int*   i;
        float* f;
    } s;
    s.p = malloc(sizeof(int[STACK_SIZE]));
    enum type st[STACK_SIZE];
    for (int c = 0; c < STACK_SIZE; c++) st[c] = int_;

    union {
        int i;
        float f;
    } op1, op2;

    printf("start pl0\n");
    fprintf(fresult, "start pl0\n");
    memset(s.p, 0, sizeof(int[4]));

    sigset_t set;
    int sig = 0;
    sigemptyset(&set);
    sigaddset(&set, SIGCONT);

    do {
        // printf("p = %d, t = %d\n", p, t);
        // fflush(NULL);
        i = code[p];
        // printf("after code %s %d %d\n", mnemonic[code[p].f], code[p].l, code[p].a);
        // dump_stacks_to_files(s.p, st, cnt, p, t);
        p++;
        cnt++;

        if (step_mode) {
            // pause();
            sigwait(&set, &sig);
            freopen(NULL, "w", fstack);
            for (int c = 0; c <= t; c++) {
                switch (st[c]) {
                    case int_:
                    case bool_:
                    case char_:
                        fprintf(fstack, "%d\n", s.i[c]);
                        break;
                    case float_:
                        fprintf(fstack, "%f\n", s.f[c]);
                        break;
                }
            }
            printf("#p#%d\n", p - 1);
            fflush(NULL);
        }

        switch (i.f) {
            case lit:
                t++;
                s.i[t] = i.a;
                if (i.l) st[t] = float_;
                break;
            case opr:
                if (i.l && i.a != op_write) {
                    st[t - 1] = float_;
                    op1.f = (i.l & 0x1) ? s.f[t - 1] : (float)s.i[t - 1];
                    op2.f = (i.l & 0x2) ? s.f[t] : (float)s.i[t];
                } else {
                    op1.i = s.i[t - 1];
                    op2.i = s.i[t];
                }
                switch (i.a) {
                    case op_ret:
                        t = b - 1 - s.i[b - 1];
                        p = s.i[b - 1 + 3];
                        b = s.i[b - 1 + 2];
                        break;
                    case op_rev:
                        if (i.l) s.f[t] = -s.f[t]; else s.i[t] = -s.i[t];
                        break;
                    case op_odd:
                        s.i[t] %= 2;
                        break;
                    case op_add:
                        t--;
                        if (i.l) s.f[t] = op1.f + op2.f; else s.i[t] = op1.i + op2.i;
                        break;
                    case op_sub:
                        t--;
                        if (i.l) s.f[t] = op1.f - op2.f; else s.i[t] = op1.i - op2.i;
                        break;
                    case op_mul:
                        t--;
                        if (i.l) s.f[t] = op1.f * op2.f; else s.i[t] = op1.i * op2.i;
                        break;
                    case op_div:
                        t--;
                        if (i.l) s.f[t] = op1.f / op2.f; else s.i[t] = op1.i / op2.i;
                        break;
                    case op_mod:
                        t--;
                        if (i.l) runtime_error(INVALID_OPERAND); else s.i[t] = op1.i % op2.i;
                        break;
                    case op_eq:
                        t--;
                        s.i[t] = i.l ? op1.f == op2.f : op1.i == op2.i;
                        break;
                    case op_neq:
                        t--;
                        s.i[t] = i.l ? op1.f != op2.f : op1.i != op2.i;
                        break;
                    case op_lt:
                        t--;
                        s.i[t] = i.l ? op1.f < op2.f : op1.i < op2.i;
                        break;
                    case op_gte:
                        t--;
                        s.i[t] = i.l ? op1.f >= op2.f : op1.i >= op2.i;
                        break;
                    case op_gt:
                        t--;
                        s.i[t] = i.l ? op1.f > op2.f : op1.i > op2.i;
                        break;
                    case op_lte:
                        t--;
                        s.i[t] = i.l ? op1.f <= op2.f : op1.i <= op2.i;
                        break;
                    case op_lor:
                        t--;
                        s.i[t] = op1.i || op2.i;
                        break;
                    case op_land:
                        t--;
                        s.i[t] = op1.i && op2.i;
                        break;
                    case op_bor:
                        t--;
                        s.i[t] = op1.i | op2.i;
                        break;
                    case op_xor:
                        t--;
                        s.i[t] = op1.i ^ op2.i;
                        break;
                    case op_band:
                        t--;
                        s.i[t] = op1.i & op2.i;
                        break;
                    case op_cast:
                        switch (i.l) {
                            case itof:
                                st[t] = float_;
                                s.f[t] = (float)s.i[t];
                                break;
                            case ftoi:
                                st[t] = int_;
                                s.i[t] = (int)s.f[t];
                                break;
                            default:
                                runtime_error(UNKNOWN_OPERATOR);
                                break;
                        }
                        break;
                    case op_write:
                        switch(i.l) {
                            case io_int:
                                printf("%d", s.i[t]);
                                fprintf(fresult, "%d", s.i[t]);
                                break;
                            case io_char:
                                printf("%c", s.i[t]);
                                fprintf(fresult, "%c", s.i[t]);
                                break;
                            case io_bool:
                                printf(s.i[t] ? "true" : "false");
                                fprintf(fresult, s.i[t] ? "true" : "false");
                                break;
                            case io_float:
                                printf("%f", s.f[t]);
                                fprintf(fresult, "%f", s.f[t]);
                                break;
                        }
                        t--;
                        break;
                    case op_lf:
                        printf("\n");
                        fprintf(fresult, "\n");
                        break;
                    case op_read:
                        t++;
                        printf("?");
                        fprintf(fresult, "?");
                        switch(i.l) {
                            case io_int:
                                scanf("%d", &s.i[t]);
                                fprintf(fresult, "%d\n", s.i[t]);
                                break;
                            case io_char:
                                scanf(" %c", &buffer[0]);
                                s.i[t] = (int)buffer[0];
                                fprintf(fresult, "%c\n", s.i[t]);
                                break;
                            case io_bool:
                                scanf("%s", buffer);
                                if (strcmp(buffer, "true") == 0) s.i[t] = 1;
                                else if (strcmp(buffer, "false") == 0) s.i[t] = 0;
                                else runtime_error(INVALID_INPUT);
                                fprintf(fresult, "%s\n", s.i[t] ? "true" : "false");
                                break;
                            case io_float:
                                st[t] = float_;
                                scanf("%f", &s.f[t]);
                                fprintf(fresult, "%f\n", s.f[t]);
                                break;
                        }
                        break;
                }
                break;
            case lod:
                t += 1;
                s.i[t] = s.i[base(i.l, s.i, b) + i.a];
                st[t] = st[base(i.l, s.i, b) + i.a];
                break;
            case ldx:
                s.i[t] = s.i[base(i.l, s.i, b) + i.a + s.i[t]];
                break;
            case sto:
                s.i[base(i.l, s.i, b) + i.a] = s.i[t];
                st[base(i.l, s.i, b) + i.a] = st[t];
                t -= 1;
                break;
            case stx:
                s.i[base(i.l, s.i, b) + i.a + s.i[t - 1]] = s.i[t];
                t -= 2;
                break;
            case cal:
                s.i[t + 1] = base(i.l, s.i, b);
                s.i[t + 2] = b;
                s.i[t + 3] = p;
                b = t + 1;
                p = i.a;
                break;
            case ini:
                t += i.a;
                break;
            case jmp:
                p = i.a;
                break;
            case jpc:
                if (s.i[t] == 0) p = i.a;
                t -= 1;
                break;
        }
    } while (p != 0);
    printf("end pl0\n");
    fprintf(fresult, "end pl0\n");
}

int base(int l, int *s, int b) {
    int b1;
    b1 = b;
    while (l > 0) {
        b1 = s[b1];
        l--;
    }
    return b1;
}
