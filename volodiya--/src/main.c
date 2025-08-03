#include "lexer.h"
#include "parser.c"

int main() {
    parse(lexer_all("../tests/test.vol"));
    return 0;
}