#include <stdlib.h>
#include <stdio.h>
#include "x0.h"
extern int err;

int main(int argc, char *argv[]){
    if (argc != 4) {
        fprintf(stderr, "usage: ./x0 [source] [output_path] [mode]");
        exit(1);
    }
    compile_and_run(argv[1], argv[2], !strcmp(argv[3], "1"));
    return err;
}
