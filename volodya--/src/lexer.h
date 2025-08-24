#pragma once
#include "Token.h"
#include "TokenVec.h"

typedef struct Lexer Lexer;

void  lexer_init(Lexer *const lex, const char *s, size_t len);

Token lexer_next(Lexer *const lexer);

TokenVec lexer_all(const char *path);

void dump_lex_errors(const Lexer *const lex);

//void  lexer_free(Lexer *lx);