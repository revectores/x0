#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <signal.h>
#include "x0.h"

char symbol_words[SYM_CNT][10] = {
        "nul",       "ident",    "number",    "plus",     "minus",
        "times",     "slash",    "odd_sym",   "eql",      "neq",
        "lss",       "geq",      "gtr",       "leq",      "lparen",
        "rparen",    "comma",    "semicolon", "period",   "becomes",
        "begin_sym", "end_sym",  "if_sym",    "then_sym", "while_sym",
        "write_sym", "read_sym", "do_sym",    "call_sym", "const_sym",
        "var_sym",   "proc_sym", "main_sym",  "type_sym", "lbracket",
        "rbracket",  "else_sym", "mod",       "not_sym",  "lor",
        "land",      "bor",      "band",      "bxor",     "for_sym",
};

char type_word[NTYPE][10] = {
        "bool", "char", "float", "int"
};

void dump_set(const bool *s) {
    for (int i = 0; i < SYM_CNT; i++) if (s[i]) printf("%s ", symbol_words[i]);
    printf("\n");
}

void dump_sym() {
    printf("current sym: %s\n", symbol_words[sym]);
}

void compile_and_run(char *source, char *path, bool step_mode) {
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
    else error(300);

    addset(nxtlev, declbegsys, statbegsys, SYM_CNT);
    nxtlev[end_sym] = true;

    block(0, 0, nxtlev);

    if (sym != end_sym) error(9);
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
    strcpy(&(word[4][0]), "for");
    strcpy(&(word[5][0]), "if");
    strcpy(&(word[6][0]), "main");
    strcpy(&(word[7][0]), "odd");
    strcpy(&(word[8][0]), "procedure");
    strcpy(&(word[9][0]), "read");
    strcpy(&(word[10][0]), "then");
    strcpy(&(word[11][0]), "var");
    strcpy(&(word[12][0]), "while");
    strcpy(&(word[13][0]), "write");

    wsym[0] = call_sym;
    wsym[1] = const_sym;
    wsym[2] = do_sym;
    wsym[3] = else_sym;
    wsym[4] = for_sym;
    wsym[5] = if_sym;
    wsym[6] = main_sym;
    wsym[7] = odd_sym;
    wsym[8] = proc_sym;
    wsym[9] = read_sym;
    wsym[10] = then_sym;
    wsym[11] = var_sym;
    wsym[12] = while_sym;
    wsym[13] = write_sym;

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
    declbegsys[proc_sym]  = true;

    statbegsys[begin_sym] = true;
    statbegsys[call_sym]  = true;
    statbegsys[if_sym]    = true;
    statbegsys[while_sym] = true;
    statbegsys[read_sym]  = true;
    statbegsys[write_sym] = true;
    statbegsys[ident]     = true;
    statbegsys[for_sym]   = true;

    factbegsys[ident]  = true;
    factbegsys[number] = true;
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
    if (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')) {
        k = 0;
        do {
            if (k < AL) {
                A[k] = ch;
                k++;
            }
            getch();
        } while (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ('0' <= ch && ch <= '9'));
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
        num = 0;
        sym = number;
        do {
            num = 10 * num + ch - '0';
            k++;
            getch();
        } while (ch >= '0' && ch <= '9');
        k--;
        if (k > NMAX) error(30);
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

    if (a >= MAX_ADDR) {
        printf("address overflow\n");
        exit(5);
    }
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
    bool nxtlev[SYM_CNT];

    table[tx].adr = cx;
    gen(jmp, 0, 0);
    if (lev > MAX_LEVEL) error(32);

    if (sym == begin_sym) getsym(); else error(100);

    do {
        while (sym == const_sym) {
            getsym();
            if (sym == type_sym) getsym();
            else error(301);
            const_decl(&tx, lev, &dx);
            if (sym == semicolon) getsym();
            else error(5);
        }

        while (sym == type_sym) {
            getsym();
            var_decl(&tx, lev, &dx);
            if (sym == semicolon) getsym();
            else error(5);
        }

        while (sym == proc_sym) {
            getsym();
            if (sym == ident) {
                enter(procedure, &tx, lev, &dx);
                getsym();
            } else error(4);

            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            block(lev + 1, tx, nxtlev);

            if (sym == end_sym) {
                getsym();
                memcpy(nxtlev, statbegsys, sizeof(bool[SYM_CNT]));
                nxtlev[ident] = true;
                nxtlev[proc_sym] = true;
                test(nxtlev, fsys, 6);
            } else error(5);
        }

        memcpy(nxtlev, statbegsys, sizeof(bool[SYM_CNT]));
        nxtlev[ident] = true;
        nxtlev[end_sym] = true;
        test(nxtlev, declbegsys, 7);
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
                case procedure:
                    printf("    %d proc %s ", i, table[i].name);
                    printf("lev = %d, addr = %d, size = %d\n", table[i].level, table[i].adr, table[i].size);
                    fprintf(ftable, "    %d proc %s ", i, table[i].name);
                    fprintf(ftable, "lev = %d, addr = %d, size = %d\n", table[i].level, table[i].adr, table[i].size);
                    break;
            }
            printf("\n");
            fprintf(ftable, "\n");
        }
    }

    memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
    nxtlev[semicolon] = true;
    nxtlev[end_sym]   = true;
    statement(nxtlev, &tx, lev);

    while (inset(sym, statbegsys)) {
        statement(nxtlev, &tx, lev);
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
            if (num > MAX_ADDR) {
                error(31);
                num = 0;
            }
            table[*ptx].val = num;
            table[*ptx].type = type;
            table[*ptx].size = 0;
            break;
        case variable:
            table[*ptx].level = lev;
            table[*ptx].adr = *pdx;
            table[*ptx].type = type;
            table[*ptx].size = size;
            *pdx += size ? size : 1;
            break;
        case procedure:
            table[*ptx].level = lev;
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
    enum type levels[NTYPE] = {bool_, char_, int_, float_};
    int n = NTYPE;
    while (n--) if (t1 == levels[n] || t2 == levels[n]) return levels[n];
    return bool_;
}

void const_decl(int *ptx, int lev, int *pdx) {
    if (sym == ident) {
        getsym();
        if (sym == becomes) {
            getsym();
            if (sym == number) {
                enter(constant, ptx, lev, pdx);
                getsym();
            } else {
                error(2);
            }
        } else {
            error(3);
        }
    } else {
        error(4);
    }
}

void var_decl(int *ptx, int lev, int *pdx) {
    if (sym == ident) {
        getsym();
        if (sym == lbracket) {
            getsym();
            if (sym == number) {
                size = num;
                getsym();
                if (sym == rbracket) {
                    getsym();
                }
                else error(303);
            } else error(2);
        } else {
            size = 0;
        }
        enter(variable, ptx, lev, pdx);

    } else {
        error(4);
    }
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
            else error(5);
            break;
        case read_sym:
            getsym();
            if (sym == ident) i = position(id, *ptx);
            else i = 0;
            if (i == 0) error(35);
            else {
                gen(opr, table[i].type, op_read);
                gen(sto, lev - table[i].level, table[i].adr);
            }
            getsym();
            if (sym == semicolon) getsym();
            else error(5);
            break;
        case write_sym:
            getsym();
            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            t = clause_or(nxtlev, ptx, lev);
            gen(opr, t, op_write);
            gen(opr, 0, op_lf);
            if (sym == semicolon) getsym();
            else error(5);
            break;
        case call_sym:
            getsym();
            if (sym != ident) error(14);
            else {
                i = position(id, *ptx);

                if (i == 0) error(11);
                else {
                    if (table[i].kind == procedure) gen(cal, lev - table[i].level, table[i].adr);
                    else error(15);
                }
                getsym();
                if (sym == semicolon) getsym();
                else error(5);
            }
            break;
        case if_sym:
            getsym();
            if (sym == lparen) getsym();
            else error(50);

            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            nxtlev[rparen] = true;
            clause_or(nxtlev, ptx, lev);

            if (sym == rparen) getsym(); else error(51);
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
            else error(17);
            break;
        case while_sym:
            cx1 = cx;
            getsym();
            if (sym == lparen) getsym();
            else error(50);

            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            nxtlev[rparen] = true;
            clause_or(nxtlev, ptx, lev);
            if (sym == rparen) getsym(); else error(51);
            cx2 = cx;
            gen(jpc, 0, 0);

            statement(fsys, ptx, lev);
            gen(jmp, 0, cx1);
            code[cx2].a = cx;
            break;
        case for_sym:
            getsym();
            if (sym == lparen) getsym();
            else error(50);

            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            nxtlev[semicolon] = true;
            expression(nxtlev, ptx, lev);
            cx1 = cx;
            if (sym == semicolon) getsym(); else error(5);
            clause_or(nxtlev, ptx, lev);
            if (sym == semicolon) getsym(); else error(5);
            cx2 = cx;
            gen(jpc, 0, 0);
            cx3 = cx;
            gen(jmp, 0, 0);
            nxtlev[semicolon] = false;
            nxtlev[rparen] = true;
            cx4 = cx;
            expression(nxtlev, ptx, lev);
            gen(jmp, 0, cx1);
            if (sym == rparen) getsym(); else error(51);

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
    int i;
    bool is_array;
    bool nxtlev[SYM_CNT];
    i = position(id, *ptx);

    if (i == 0) error(11);
    else {
        if (table[i].kind != variable) {
            error(12);
            i = 0;
        } else {
            this_type = table[i].type;
            getsym();
            is_array = false;
            if (sym == lbracket) {
                is_array = true;
                if (table[i].size) {
                    getsym();
                    memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
                    nxtlev[rparen] = true;
                    nxtlev[rbracket] = true;
                    enum type t = clause_or(nxtlev, ptx, lev);
                    if (t == float_) error(600);
                    if (sym == rbracket) getsym();
                    else error(303);
                } else error(500);
            }

            if (sym == becomes) {
                getsym();
                memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
                enum type t = clause_or(nxtlev, ptx, lev);
                if (this_type == float_ && t != float_) gen(opr, itof, op_cast);
                if (this_type != float_ && t == float_) gen(opr, ftoi, op_cast);
                if (is_array) {
                    gen(stx, lev - table[i].level, table[i].adr);
                } else {
                    gen(sto, lev - table[i].level, table[i].adr);
                }
            } else error(17);
        }
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
        if (this_type == float_) error(601);
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
        if (this_type == float_) error(601);
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
        if (this_type == float_) error(601);
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
                error(404);
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
        case number:
        case lparen:
            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            this_type = factor(nxtlev, ptx, lev);
            break;
        case not_sym:
            getsym();
            gen(lit, 0, 1);
            memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
            enum type t = factor(nxtlev, ptx, lev);
            if (t != bool_) error(300);
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
            error(301);
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
                if (i == 0) error(11);
                else {
                    switch (table[i].kind) {
                        case constant:
                            gen(lit, 0, table[i].val);
                            this_type = table[i].type;
                            break;
                        case variable:
                            if (table[i].size) {
                                getsym();
                                if (sym == lbracket) {
                                    getsym();
                                    memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
                                    nxtlev[rparen] = true;
                                    nxtlev[rbracket] = true;
                                    clause_or(nxtlev, ptx, lev);
                                    if (sym != rbracket) error(303);
                                    gen(ldx, lev - table[i].level, table[i].adr);
                                } else error(302);
                            } else {
                                printf("lod %d %d\n", lev - table[i].level, table[i].adr);
                                gen(lod, lev - table[i].level, table[i].adr);
                            }
                            this_type = table[i].type;
                            break;
                        case procedure:
                            error(21);
                            break;
                    }
                }
                getsym();
                break;
            case number:
                if (num > MAX_ADDR) {
                    error(31);
                    num = 0;
                }
                gen(lit, 0, num);
                this_type = int_;
                getsym();
                break;
            case lparen:
                getsym();
                memcpy(nxtlev, fsys, sizeof(bool[SYM_CNT]));
                nxtlev[rparen] = true;
                this_type = clause_or(nxtlev, ptx, lev);
                if (sym == rparen) getsym();
                else error(22);
                break;
            default:
                break;
        }

        memset(nxtlev, 0, sizeof(bool[SYM_CNT]));
        nxtlev[lparen] = true;
        test(fsys, nxtlev, 23);
    }
    return this_type;
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

void interpret(bool step_mode) {
    int p = 0;
    int b = 1;
    int t = 0;
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
        // printf("p = %d\n", p);
        // fflush(NULL);
        i = code[p];
        // printf("after code %s %d %d\n", mnemonic[code[p].f], code[p].l, code[p].a);
        p++;

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
                break;
            case opr:
                if (i.l) {
                    st[t - 1] = float_;
                    op1.f = (i.l & 0x1) ? s.f[t - 1] : (float)s.i[t - 1];
                    op2.f = (i.l & 0x2) ? s.f[t] : (float)s.i[t];
                } else {
                    op1.i = s.i[t - 1];
                    op2.i = s.i[t];
                }
                switch (i.a) {
                    case op_ret:
                        t = b - 1;
                        p = s.i[t + 3];
                        b = s.i[t + 2];
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
                        if (i.l) runtime_error(2); else s.i[t] = op1.i % op2.i;
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
                                runtime_error(3);
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
                                scanf("%s.i", buffer);
                                if (strcmp(buffer, "true") == 0) s.i[t] = 1;
                                else if (strcmp(buffer, "false") == 0) s.i[t] = 0;
                                else runtime_error(1);
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
        // dump_stack(s.i, t);
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
