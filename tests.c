#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include "x0.h"


int is_identical(char *fname1, char *fname2) {
    FILE *fp1, *fp2;
    int ch1, ch2;

    fp1 = fopen(fname1, "r");
    fp2 = fopen(fname2, "r");

    if (fp1 == NULL) {
        printf("Cannot open %s for reading ", fname1);
        exit(1);
    } else if (fp2 == NULL) {
        printf("Cannot open %s for reading ", fname2);
        exit(1);
    } else {
        ch1 = getc(fp1);
        ch2 = getc(fp2);

        while ((ch1 != EOF) && (ch2 != EOF) && (ch1 == ch2)) {
            ch1 = getc(fp1);
            ch2 = getc(fp2);
        }

        fclose(fp1);
        fclose(fp2);
    }
    return ch1 == ch2;
}

void run_test(char *name) {
    char fname[80];
    sprintf(fname, "../output/test_%s_output.txt", name);
    freopen(fname, "w", stdout);
    sprintf(fname, "../tests/test_%s/test_%s_input.txt", name, name);
    freopen(fname, "r", stdin);
    sprintf(fname, "../tests/test_%s/test_%s.pl0", name, name);
    compile_and_run(fname, ".", false);
    assert(err == 0);
    sprintf(fname, "../tests/test_%s/test_%s_fresult.txt", name, name);
    assert(is_identical(fname, "fresult.txt"));
}

int run_tests() {
    run_test("empty");
    run_test("comment");

    run_test("int_io");
    run_test("bool_io");
    run_test("char_io");
    run_test("float_io");
    run_test("cast");

    run_test("float_opr");
    run_test("bool_opr");
    run_test("bitwise");
    run_test("mod");
    run_test("odd");
    run_test("expression");

    run_test("for");
    run_test("proc");
    return 0;
}

int main() {
    run_tests();
}
