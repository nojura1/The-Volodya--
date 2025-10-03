#include "lexer/lexer.h"
#include "parser/parser.h"

int main(void) {
    parse(lexer_all("../tests/test.vol"));
    return 0;
}