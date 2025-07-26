#pragma once
#include <stddef.h>

typedef enum {
    // --- Special ---
    TOK_EOF,
    TOK_UNKNOWN,

    // --- Identifiers & Literals ---
    TOK_IDENT,
    TOK_DEC_LIT,
    TOK_HEX_LIT,
    TOK_BIN_LIT,
    TOK_OCT_LIT,
    TOK_BOOL_LIT,
    TOK_STRING_LIT,

    // --- Keywords (statements / types) ---
    TOK_KW_FN,
    TOK_KW_STRUCT,
    TOK_KW_IF,
    TOK_KW_ELSE,
    TOK_KW_WHILE,
    TOK_KW_FOR,
    TOK_KW_RETURN,
    TOK_KW_BREAK,
    TOK_KW_CONST,
    TOK_KW_VOLOID,
    TOK_KW_I32,
    TOK_KW_I64,
    TOK_KW_U32,
    TOK_KW_U64,
    TOK_KW_BOOL,
    TOK_KW_STRING,
    TOK_KW_TRUE,
    TOK_KW_FALSE,

    // --- Operators (single-char arithmetic) ---
    TOK_PLUS,        // +
    TOK_MINUS,       // -
    TOK_STAR,        // *
    TOK_SLASH,       // /
    TOK_PERCENT,     // %

    // --- Operators (comparisons) ---
    TOK_EQ_EQ,       // ==
    TOK_NOT_EQ,      // !=
    TOK_GT,          // >
    TOK_LT,          // <
    TOK_GT_EQ,       // >=
    TOK_LT_EQ,       // <=

    // --- Operators (logical) ---
    TOK_AMP_AMP,     // &&
    TOK_PIPE_PIPE,   // ||
    TOK_NOT,         // !

    // --- Assignment ---
    TOK_EQ,          // =

    // --- Separators ---
    TOK_LPAREN,      // (
    TOK_RPAREN,      // )
    TOK_LBRACKET,    // [
    TOK_RBRACKET,    // ]
    TOK_LBRACE,      // {
    TOK_RBRACE,      // }
    TOK_COMMA,       // ,
    TOK_DOT,         // .
    TOK_COLON,       // :
    TOK_SEMICOLON,   // ;

    TOK__COUNT
} TokenType;

typedef struct {
    TokenType type;
    const char *start_pos;
    size_t length;
    size_t line;
    size_t column;
} Token;