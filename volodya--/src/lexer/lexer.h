#pragma once
#include "../Token/TokenVec.h"
#include "../Token/TokenNames.h"

typedef struct Lexer Lexer;

void lexer_init(Lexer *const lex, const char *s, size_t len);

Token lexer_next(Lexer *const lexer);

TokenVec lexer_all(const char *path);

void dump_lex_errors(const Lexer *const lex, const char *path);

//void  lexer_free(Lexer *lx);