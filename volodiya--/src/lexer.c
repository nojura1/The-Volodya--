#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include "lexer.h"
#include "TokenVec.h"

typedef struct {
    size_t line;
    size_t column;
    const char *msg;
} LexError;

typedef struct {
    const char *lex;
    TokenType kind;
} Keyword;

typedef struct Lexer {
    const char *src;
    size_t length;
    size_t pos;
    size_t line;
    size_t column;
    LexError *errors;
    size_t err_count;
    size_t err_cap;
    int had_error;
} Lexer;

static const Keyword keywords[] = {
    {"fn",     TOK_KW_FN},
    {"struct", TOK_KW_STRUCT},
    {"if",     TOK_KW_IF},
    {"else",   TOK_KW_ELSE},
    {"while",  TOK_KW_WHILE},
    {"for",    TOK_KW_FOR},
    {"return", TOK_KW_RETURN},
    {"break",  TOK_KW_BREAK},
    {"const",  TOK_KW_CONST},
    {"voloid",   TOK_KW_VOLOID},
    {"i32",    TOK_KW_I32},
    {"i64",    TOK_KW_I64},
    {"u32",    TOK_KW_U32},
    {"u64",    TOK_KW_U64},
    {"bool",   TOK_KW_BOOL},
    {"string", TOK_KW_STRING},
    {"true",   TOK_KW_TRUE},
    {"false",  TOK_KW_FALSE},
};

static const char *TOKEN_NAMES[TOK__COUNT] = {
    /* --- Special --- */
    "TOK_EOF",
    "TOK_UNKNOWN",

    /* --- Identifiers & Literals --- */
    "TOK_IDENT",
    "TOK_DEC_LIT",
    "TOK_HEX_LIT",
    "TOK_BIN_LIT",
    "TOK_OCT_LIT",
    "TOK_BOOL_LIT",
    "TOK_STRING_LIT",

    /* --- Keywords (statements / types) --- */
    "TOK_KW_FN",
    "TOK_KW_STRUCT",
    "TOK_KW_IF",
    "TOK_KW_ELSE",
    "TOK_KW_WHILE",
    "TOK_KW_FOR",
    "TOK_KW_RETURN",
    "TOK_KW_BREAK",
    "TOK_KW_CONST",
    "TOK_KW_VOLOID",
    "TOK_KW_I32",
    "TOK_KW_I64",
    "TOK_KW_U32",
    "TOK_KW_U64",
    "TOK_KW_BOOL",
    "TOK_KW_STRING",
    "TOK_KW_TRUE",
    "TOK_KW_FALSE",

    /* --- Operators (single-char arithmetic) --- */
    "TOK_PLUS",        // +
    "TOK_MINUS",       // -
    "TOK_STAR",        // *
    "TOK_SLASH",       // /
    "TOK_PERCENT",     // %

    /* --- Operators (comparisons) --- */
    "TOK_EQ_EQ",       // ==
    "TOK_NOT_EQ",      // !=
    "TOK_GT",          // >
    "TOK_LT",          // <
    "TOK_GT_EQ",       // >=
    "TOK_LT_EQ",       // <=

    /* --- Operators (logical) --- */
    "TOK_AMP_AMP",     // &&
    "TOK_PIPE_PIPE",   // ||
    "TOK_NOT",         // !

    /* --- Assignment --- */
    "TOK_EQ",          // =

    /* --- Separators --- */
    "TOK_LPAREN",      // (
    "TOK_RPAREN",      // )
    "TOK_LBRACKET",    // [
    "TOK_RBRACKET",    // ]
    "TOK_LBRACE",      // {
    "TOK_RBRACE",      // }
    "TOK_COMMA",       // ,
    "TOK_DOT",         // .
    "TOK_COLON",       // :
    "TOK_SEMICOLON",   // ;
};

const char *token_type_name(TokenType t) {
    size_t n = sizeof(TOKEN_NAMES) / sizeof(TOKEN_NAMES[0]);
    if ((size_t)t < n) return TOKEN_NAMES[t];
    return "TOK_<OUT_OF_RANGE>";
}

char *read_file(const char *path, size_t *len_out) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;

    if (fseek(f, 0, SEEK_END) != 0) { fclose(f); return NULL; }
    long end = ftell(f);
    if (end < 0) { fclose(f); return NULL; }
    rewind(f);

    size_t len = (size_t)end;
    char *buf = malloc(len + 1);
    if (!buf) { fclose(f); return NULL; }

    size_t n = fread(buf, 1, len, f);
    if (n != len) {
        if (ferror(f)) { free(buf); fclose(f); return NULL; }
        len = n;
    }

    buf[len] = '\0';
    fclose(f);
    if (len_out) *len_out = len;
    return buf;
}

char *read_file_or_exit(const char *path, size_t *len_out) {
    char *p = read_file(path, len_out);
    if (!p) {
        fprintf(stderr, "fatal: cannot read %s: %s\n", path, strerror(errno));
        exit(1);
    }
    return p;
}

static inline int at_end(const Lexer *const lexer) {
    return lexer->length <= lexer->pos;
}

static inline char peek(const Lexer *const lexer) {
    if (at_end(lexer)) return '\0';
    return lexer->src[lexer->pos];
}

static inline char peek_next(const Lexer *const lexer) {
    if (lexer->length <= lexer->pos + 1) return '\0';
    return lexer->src[lexer->pos + 1];
}

static inline char advance(Lexer *const lexer) {
    if (at_end(lexer)) return '\0';
    if (lexer->src[lexer->pos] == '\n') { lexer->line++; lexer->column = 1; }
    else lexer->column++;
    return lexer->src[lexer->pos++];
}

static inline int match(Lexer *const lexer, char expected) {
    if (at_end(lexer)) return 0;
    if (lexer->src[lexer->pos] == expected) { 
        advance(lexer);
        return 1;
    }
    return 0;
}

static inline Token make_token(TokenType type, const char *start, size_t l, size_t line, size_t column) {
    return (Token){type, start, l, line, column};
}

void lex_error(Lexer *const lexer, const char *msg) {
    lexer->had_error = 1;

    if (lexer->err_cap == lexer->err_count) {
        lexer->err_cap = lexer->err_cap ? lexer->err_cap * 2 : 8;
        lexer->errors = realloc(lexer->errors, sizeof(LexError) * lexer->err_cap);
        if (!lexer->errors) {
            fprintf(stderr, "realloc\n");
            exit(1);
        }
    }
    
    size_t t = lexer->err_count++;
    lexer->errors[t].msg = msg;
    lexer->errors[t].line = lexer->line;
    lexer->errors[t].column = lexer->column;
}

void skip_ignorable(Lexer *const lexer) {
    while (1) {
        char c = peek(lexer);
        char c1 = peek_next(lexer);
        
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            advance(lexer);
        } else if (c == '/' && c1 == '/') {
            for (; peek(lexer) != '\n' && peek(lexer) != '\0'; advance(lexer));
        } else if (c == '/' && c1 == '*') {
            for (; (peek(lexer) != '*' || peek_next(lexer) != '/') && peek(lexer) != '\0'; advance(lexer));
            if (peek(lexer) == '\0') { lex_error(lexer, "unterminated block comment"); break; }
            advance(lexer);
            advance(lexer);
        } else {
            break;
        }
    }
}

TokenType keyword_lookup(const char *lex, const size_t len) {
    for (size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); i++) {
        const char *t = keywords[i].lex;
        if (strlen(t) == len && strncmp(t, lex, len) == 0) return keywords[i].kind;
    }
    return TOK_IDENT;
}

Token scan_identifier(Lexer *const lexer) {
    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;

    if (isalpha((unsigned char) peek(lexer))) {
        for (; isalnum((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer));
    }

    size_t l = lexer->src + lexer->pos - start_pos;
    return make_token(keyword_lookup(start_pos, l), start_pos, l, line_start, column_start);
}

Token scan_hex(Lexer *const lexer) {
    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;
    
    advance(lexer);
    advance(lexer);

    char c = peek(lexer);

    if (!isxdigit((unsigned char) c)) lex_error(lexer, "expexted hexadecimal digit after '0x'/'0X'");
    else if (c == '_') lex_error(lexer, "underscore cannot appear at the beginning of a hexadecimal literal");

    if (isxdigit((unsigned char) c)) {
        for (; isxdigit((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer));
    }

    c = peek(lexer);

    if (isalnum((unsigned char) c) || c == '_') {
        lex_error(lexer, "invalid numeric literal: unexpected characters");
        for (; isalnum((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer));
    }

    size_t l = lexer->src + lexer->pos - start_pos;
    return make_token(TOK_HEX_LIT, start_pos, l, line_start, column_start);
}

Token scan_bin(Lexer *const lexer) {
    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;
    
    advance(lexer);
    advance(lexer);
    
    char c = peek(lexer);

    if (c != '0' || c != '1') lex_error(lexer, "expexted binary digit after '0b'/'0B'");
    else if (c == '_') lex_error(lexer, "underscore cannot appear at the beginning of a binary literal");

    if (c == '0' || c == '1') {
        for (; peek(lexer) == '0' || peek(lexer) == '1' || peek(lexer) == '_'; advance(lexer));
    }

    c = peek(lexer);

    if (isalnum((unsigned char) c) || c == '_') {
        lex_error(lexer, "invalid numeric literal: unexpected characters");
        for (; isalnum((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer));
    }

    size_t l = lexer->src + lexer->pos - start_pos;
    return make_token(TOK_BIN_LIT, start_pos, l, line_start, column_start);
}

static inline int is_oct(char n) {
    return n >= '0' && n <= '7';
}

Token scan_oct(Lexer *const lexer) {
    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;

    advance(lexer);
    advance(lexer);

    char c = peek(lexer);

    if (!is_oct(c)) lex_error(lexer, "expexted octal digit after '0o'/'0O'");
    else if (c == '_') lex_error(lexer, "underscore cannot appear at the beginning of a octal literal");

    if (is_oct(c)) {
        for (; is_oct(peek(lexer)) || peek(lexer) == '_'; advance(lexer));
    }

    c = peek(lexer);

    if (isalnum((unsigned char) c) || c == '_') {
        lex_error(lexer, "invalid numeric literal: unexpected characters");
        for (; isalnum((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer));
    }

    size_t l = lexer->src + lexer->pos - start_pos;
    return make_token(TOK_OCT_LIT, start_pos, l, line_start, column_start);
}

static inline int no_zero(char n) {
    return n >= '1' && n <= '9';
}

Token scan_dec(Lexer *const lexer) {
    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;

    if (no_zero(peek(lexer))) {
        for (; isdigit((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer));
    } else advance(lexer);

    char c = peek(lexer);

    if (isalnum((unsigned char) c) || c == '_') {
        lex_error(lexer, "invalid numeric literal: unexpected characters");
        for (; isalnum((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer));
    }

    size_t l = lexer->src + lexer->pos - start_pos;
    return make_token(TOK_DEC_LIT, start_pos, l, line_start, column_start);
}

Token scan_number(Lexer *const lexer) {
    char c = peek(lexer);
    char c1 = peek_next(lexer);

    if ((c1 == 'x' || c1 == 'X') && c == '0') return scan_hex(lexer);
    else if ((c1 == 'b' || c1 == 'B') && c == '0') return scan_bin(lexer);
    else if ((c1 == 'o' || c1 == 'O') && c == '0') return scan_oct(lexer);
    else return scan_dec(lexer);
}

Token scan_string(Lexer *const lexer) {
    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;

    advance(lexer);

    while (1) {
        char c = peek(lexer);
        if (c == '\"') { advance(lexer); break; }
        
        if (c == '\\') {
            advance(lexer); 
            switch (peek(lexer)) {
                case 'n': case 't': case 'r':
                case '\"': case '\\': case '0':
                    break;
                default: lex_error(lexer, "invalid escape sequence in string literal");
            }
        }

        c = peek(lexer);

        if (c == '\n') { lex_error(lexer, "newline in string literal"); break; }
        if (c == '\0') { lex_error(lexer, "unterminated string literal"); break; }
        advance(lexer);
    }
    
    size_t l = lexer->src + lexer->pos - start_pos;
    return make_token(TOK_STRING_LIT, start_pos, l, line_start, column_start);
}

void tokenvec_init(TokenVec *v) {
    v->data = malloc(128 * sizeof(TokenVec));
    if (!v->data) {
        fprintf(stderr, "malloc\n");
        exit(1);
    }
    v->cap = 128;
    v->count = 0;
}

void tokenvec_push(TokenVec *v, Token t) {
    if (v->count == v->cap) {
        v->cap *= 2;
        v->data = realloc(v->data, v->cap * sizeof(TokenVec));
        if (!v->data) {
            fprintf(stderr, "realloc\n");
            exit(1);
        }
    }
    v->data[v->count++] = t;
}

//void tokenvec_free(TokenVec *v); TODO

Token lexer_next(Lexer *const lexer) {
    skip_ignorable(lexer);
    char c = peek(lexer);

    if (c == '\0') return make_token(TOK_EOF, lexer->src + lexer->pos, 1, lexer->line, lexer->column);

    if (isalpha((unsigned char) c)) return scan_identifier(lexer);
    else if (isdigit((unsigned char) c)) return scan_number(lexer);
    else if (c == '\"') return scan_string(lexer);

    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;

    switch (advance(lexer)) {
        case '(': return make_token(TOK_LPAREN, start_pos, 1, line_start, column_start);
        case ')': return make_token(TOK_RPAREN, start_pos, 1, line_start, column_start);
        case '[': return make_token(TOK_LBRACKET, start_pos, 1, line_start, column_start);
        case ']': return make_token(TOK_RBRACKET, start_pos, 1, line_start, column_start);
        case '{': return make_token(TOK_LBRACE, start_pos, 1, line_start, column_start);
        case '}': return make_token(TOK_RBRACE, start_pos, 1, line_start, column_start);
        case '.': return make_token(TOK_DOT, start_pos, 1, line_start, column_start);
        case ',': return make_token(TOK_COMMA, start_pos, 1, line_start, column_start);
        case ':': return make_token(TOK_COLON, start_pos, 1, line_start, column_start);
        case ';': return make_token(TOK_SEMICOLON, start_pos, 1, line_start, column_start);
        case '+': return make_token(TOK_PLUS, start_pos, 1, line_start, column_start);
        case '-': return make_token(TOK_MINUS, start_pos, 1, line_start, column_start);
        case '*': return make_token(TOK_STAR, start_pos, 1, line_start, column_start);
        case '/': return make_token(TOK_SLASH, start_pos, 1, line_start, column_start);
        case '%': return make_token(TOK_PERCENT, start_pos, 1, line_start, column_start);
        case '=':
            if (match(lexer, '=')) return make_token(TOK_EQ_EQ, start_pos, 2, line_start, column_start);
            else return make_token(TOK_EQ, start_pos, 1, line_start, column_start);
        case '<':
            if (match(lexer, '=')) return make_token(TOK_LT_EQ, start_pos, 2, line_start, column_start);
            else return make_token(TOK_LT, start_pos, 1, line_start, column_start);
        case '>':
            if (match(lexer, '=')) return make_token(TOK_GT_EQ, start_pos, 2, line_start, column_start);
            else return make_token(TOK_GT, start_pos, 1, line_start, column_start);
        case '!':
            if (match(lexer, '=')) return make_token(TOK_NOT_EQ, start_pos, 2, line_start, column_start);
            else return make_token(TOK_NOT, start_pos, 1, line_start, column_start);
        case '&':
            if (match(lexer, '&')) return make_token(TOK_AMP_AMP, start_pos, 2, line_start, column_start);
            break;
        case '|':
            if (match(lexer, '|')) return make_token(TOK_PIPE_PIPE, start_pos, 2, line_start, column_start);
            else break;
        default:
            lex_error(lexer, "unexpected character");
            return make_token(TOK_UNKNOWN, start_pos, 1, line_start, column_start);
    }
}

void lexer_init(Lexer *const lex, const char *s, size_t len) {
    lex->src = s;
    lex->length = len;
    lex->pos = 0;
    lex->line = 1;
    lex->column = 1;
    lex->errors = NULL;
    lex->err_count = 0;
    lex->err_cap = 0;
    lex->had_error = 0;
}

void dump_lex_errors(const Lexer *const lex) {
    for (int i = 0; i < lex->err_count; i++) {
        printf("%50s  line:%5zu  column:%5zu\n", lex->errors[i].msg, lex->errors[i].line, lex->errors[i].column);
    }
}

TokenVec lexer_all(const char *path) {
    size_t len = 0;
    char *src = read_file_or_exit(path, &len);
    Lexer lexer;
    lexer_init(&lexer, src, len);
    TokenVec vec; 
    tokenvec_init(&vec);

    for (;;) {
        Token tok = lexer_next(&lexer);
        tokenvec_push(&vec, tok);

        printf("type:%20s  start:%30.*s  line:%5zu  column:%5zu\n",
               token_type_name(tok.type), (int)tok.length, tok.start_pos,
               tok.line, tok.column);

        if (tok.type == TOK_EOF) break;
    }

    if (lexer.had_error) dump_lex_errors(&lexer); exit(1);
    return vec;
}