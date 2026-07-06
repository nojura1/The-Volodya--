#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "codegen/codegen.h"
#include "semantic/semantic.h"
#include "parser/parser.h"
#include "lexer/lexer.h"

bool ends_with(char *s, char *suffix) {
    size_t s_len = strlen(s);
    size_t suffix_len = strlen(suffix);

    if (s_len < suffix_len) {
        return false;
    }

    return strcmp(s + s_len - suffix_len, suffix) == 0;
}

void compile_to_nasm_file(char *input_path, char *asm_path) {
    FILE *out = fopen(asm_path, "w");
    if (out == NULL) {
        fprintf(stderr, "vmm: cannot open output asm file: %s\n", asm_path);
        exit(EXIT_FAILURE);
    }
    
    Node *n = parse(lexer_all(input_path), input_path);
    sema(n, input_path);
    codegen_program(n, out);

    fclose(out);
}

int main(int argc, char *argv[argc + 1]) {
    char *input = NULL;
    char *output = NULL;
    bool emit_nasm = false;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-nasm") == 0 || strcmp(argv[i], "-S") == 0) {
            emit_nasm = true;
        } else if (strcmp(argv[i], "-o") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "vmm: expected filename after -o\n");
                return EXIT_FAILURE;
            }
            output = argv[i + 1];
            i++;
        } else {
            if (input != NULL) {
                fprintf(stderr, "vmm: multiple input files are not supported\n");
                return EXIT_FAILURE;
            }
            input = argv[i];
        }
    }

    if (input == NULL) {
        fprintf(stderr, "usage: vmm [-S|-nasm] input.vol [-o output]\n");
        return EXIT_FAILURE;
    }
    
    if (!ends_with(input, ".vol")) {
        fprintf(stderr, "vmm: expected input file with .vol extension\n");
        return EXIT_FAILURE;
    }

    if (output == NULL) {
        output = emit_nasm ? "out.asm" : "a.out";
    }

    char *asm_path = emit_nasm ? output : "/tmp/vmm_out.asm";
    char *obj_path = "/tmp/vmm_out.o";

    compile_to_nasm_file(input, asm_path);

    if (emit_nasm) {
        return EXIT_SUCCESS;
    }

    char cmd[1024];

    snprintf(cmd, sizeof(cmd), "nasm -f elf64 %s -o %s", asm_path, obj_path);

    int code = system(cmd);
    if (code != 0) {
        fprintf(stderr, "vmm: nasm failed\n");
        return EXIT_FAILURE;
    }

    snprintf(cmd, sizeof(cmd), "ld %s -o %s", obj_path, output);

    code = system(cmd);
    if (code != 0) {
        fprintf(stderr, "vmm: ld failed\n");
        return EXIT_FAILURE;
    }

    remove(asm_path);
    remove(obj_path);

    return EXIT_SUCCESS;
}