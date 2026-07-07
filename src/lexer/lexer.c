#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

#include "lexer.h"
#include "../Token/TokenNames.h"

#define MAX_FILE_SIZE (32 * 1024 * 1024)
#define MAX_LEXEME_LEN 256
#define MAX_STRING_LITERAL_LEN (4 * 1024)
#define MAX_TOKEN_AMOUNT (64 * 1024)
#define MAX_ERRORS 16

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
    size_t length, pos;
    size_t line, column;
    LexError *errors;
    size_t err_count, err_cap;
    bool had_error;
    bool utf8_error;
    const char *path;
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
    {"voloid", TOK_KW_VOLOID},
    {"i32",    TOK_KW_I32},
    {"i64",    TOK_KW_I64},
    {"u32",    TOK_KW_U32},
    {"u64",    TOK_KW_U64},
    {"bool",   TOK_KW_BOOL},
    {"string", TOK_KW_STRING},
    {"true",   TOK_KW_TRUE},
    {"false",  TOK_KW_FALSE},
};

static char *read_file(const char *path, size_t *len_out) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;

    if (fseek(f, 0, SEEK_END) != 0) { fclose(f); return NULL; }
    long end = ftell(f);
    if (end > MAX_FILE_SIZE) { fclose(f); fprintf(stderr, "fatal: file is too long %s\n", path); exit(EXIT_FAILURE); }
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

static char *read_file_or_exit(const char *path, size_t *len_out) {
    char *p = read_file(path, len_out);
    if (!p) {
        fprintf(stderr, "fatal: cannot read %s: %s\n", path, strerror(errno));
        exit(EXIT_FAILURE);
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
    return (Token){type, start, l, line, column, 0, NULL};
}

static inline void set_str_lit_length_bytes(Token *str_token, size_t length, char *lit) {
    str_token->lit_length_bytes = length;
    str_token->lit = lit;
}

static void dump_lex_errors(const Lexer *const lex) {
    for (size_t i = 0; i < lex->err_count; i++) {
        fprintf(stderr, "%s:%zu:%zu: lexical error: %s\n", lex->path, lex->errors[i].line, lex->errors[i].column, lex->errors[i].msg);
    }
}

static void lex_error(Lexer *const lexer, const char *msg, size_t line, size_t column) {
    lexer->had_error = true;

    if (lexer->err_cap == lexer->err_count) {
        lexer->err_cap = lexer->err_cap ? lexer->err_cap * 2 : 8;
        lexer->errors = realloc(lexer->errors, sizeof(LexError) * lexer->err_cap);
        if (!lexer->errors) {
            fprintf(stderr, "realloc\n");
            exit(EXIT_FAILURE);
        }
    }
    if (lexer->err_count > MAX_ERRORS) {
        dump_lex_errors(lexer);
        exit(EXIT_FAILURE);
    }
    size_t t = lexer->err_count++;
    lexer->errors[t].msg = msg;
    lexer->errors[t].line = line;
    lexer->errors[t].column = column;
}

static void skip_ignorable(Lexer *const lexer) {
    for (;;) {
        char c = peek(lexer);
        char c1 = peek_next(lexer);
        
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            advance(lexer);
            lexer->utf8_error = false;
        } else if (c == '/' && c1 == '/') {
            for (; peek(lexer) != '\n' && peek(lexer) != '\0'; advance(lexer));
        } else if (c == '/' && c1 == '*') {
            for (; (peek(lexer) != '*' || peek_next(lexer) != '/') && peek(lexer) != '\0'; advance(lexer));
            if (peek(lexer) == '\0') {
                lex_error(lexer, "unterminated block comment", lexer->line, lexer->column);
                break;
            }
            advance(lexer);
            advance(lexer);
        } else {
            break;
        }
    }
}

static TokenType keyword_lookup(const char *lex, const size_t len) {
    for (size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); i++) {
        const char *t = keywords[i].lex;
        if (strlen(t) == len && strncmp(t, lex, len) == 0) return keywords[i].kind;
    }
    return TOK_IDENT;
}

static Token scan_identifier(Lexer *const lexer) {
    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;

    if (isalpha((unsigned char) peek(lexer))) {
        for (size_t i = 0; isalnum((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer), ++i) {
            if (i == MAX_LEXEME_LEN) lex_error(lexer, "too long identifier", lexer->line, lexer->column);
        }
    }

    ptrdiff_t l = lexer->src + lexer->pos - start_pos;
    return make_token(keyword_lookup(start_pos, (size_t)l), start_pos, (size_t)l, line_start, column_start);
}

static Token scan_with_prefix(Lexer *const lexer, int (*fun)(int), TokenType tt) {
    advance(lexer);
    advance(lexer);

    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;
    char c = peek(lexer);

    if (c == '_') lex_error(lexer, "underscore cannot appear at the beginning of a numeric literal", lexer->line, lexer->column);
    else if (!fun((unsigned char) c)) lex_error(lexer, "expected digit after prefix", lexer->line, lexer->column);

    for (size_t i = 0; fun((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer), ++i) {
        if (i == MAX_LEXEME_LEN) lex_error(lexer, "too long numeric literal", lexer->line, lexer->column);
    }

    if (isalnum((unsigned char) peek(lexer)) || peek(lexer) == '_') {
        lex_error(lexer, "invalid numeric literal: unexpected characters", lexer->line, lexer->column);
        for (; isalnum((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer));
    }

    ptrdiff_t l = lexer->src + lexer->pos - start_pos;
    return make_token(tt, start_pos, (size_t)l, line_start, column_start);
}

static inline int is_oct(int n) { return n >= '0' && n <= '7'; }
static inline int is_bin(int n) { return n == '0' || n == '1'; }
static inline int no_zero(int n) { return n >= '1' && n <= '9'; }

static Token scan_dec(Lexer *const lexer) {
    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;

    if (no_zero(peek(lexer))) {
        for (size_t i = 0; isdigit((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer), ++i) {
            if (i == MAX_LEXEME_LEN) lex_error(lexer, "too long numeric literal", lexer->line, lexer->column); 
        }
    } else advance(lexer);

    char c = peek(lexer);

    if (isalnum((unsigned char) c) || c == '_') {
        lex_error(lexer, "invalid numeric literal: unexpected characters", lexer->line, lexer->column);
        for (; isalnum((unsigned char) peek(lexer)) || peek(lexer) == '_'; advance(lexer));
    }

    ptrdiff_t l = lexer->src + lexer->pos - start_pos;
    return make_token(TOK_DEC_LIT, start_pos, (size_t)l, line_start, column_start);
}

static Token scan_number(Lexer *const lexer) {
    char c = peek(lexer);
    char c1 = peek_next(lexer);

    if ((c1 == 'x' || c1 == 'X') && c == '0') return scan_with_prefix(lexer, isxdigit, TOK_HEX_LIT);
    else if ((c1 == 'b' || c1 == 'B') && c == '0') return scan_with_prefix(lexer, is_bin, TOK_BIN_LIT);
    else if ((c1 == 'o' || c1 == 'O') && c == '0') return scan_with_prefix(lexer, is_oct, TOK_OCT_LIT);
    else return scan_dec(lexer);
}

static inline void advance_bytes_no_col(Lexer *const lexer, size_t n) { lexer->pos += n; }

static int utf8_handler(Lexer *lexer) {
    unsigned char b0 = (unsigned char)peek(lexer);
    
    int len = (b0 < 0x80) ? 1 :
              (b0 & 0xE0) == 0xC0 ? 2 :
              (b0 & 0xF0) == 0xE0 ? 3 :
              (b0 & 0xF8) == 0xF0 ? 4 : -1;

    if (len == -1 || lexer->length - lexer->pos < (size_t)len) {
        return -1;
    }

    for (int i = 1; i < len; i++) {
        unsigned char bi = (unsigned char)lexer->src[lexer->pos + (size_t)i];
        if ((bi & 0xC0) != 0x80) {
            return -1;
        }
    }
    return len;
}

static void flush_until_quote_or_eol(Lexer *lexer) {
    while (true) {
        unsigned char c = (unsigned char)peek(lexer);
        if (c == '"' || c == '\n' || c == '\0') {
            if (c == '"') advance(lexer);
            break;
        }
        int len = utf8_handler(lexer);
        
        if (len < 1) {
            len = 1;
        }

        size_t remain = lexer->length - lexer->pos;
        if (remain < (size_t)len) {
            advance_bytes_no_col(lexer, remain);
            break;
        } else {
            advance(lexer);
            advance_bytes_no_col(lexer, (size_t)(len - 1));
        }
    }
}

static Token scan_string(Lexer *lexer) {
    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;
    size_t str_lit_len_bytes = 0;

    char *buf = malloc(MAX_STRING_LITERAL_LEN + 5);
    if (!buf) { fprintf(stderr, "malloc"); exit(EXIT_FAILURE); }

    advance(lexer);
    
    while (true) {
        int len = utf8_handler(lexer);

        if (len == 1) {
            if (peek(lexer) == '"') {
                advance(lexer);
                break;
            } 
            if (peek(lexer) == '\n' || peek(lexer) == '\0') {
                lex_error(lexer, "unterminated string literal", lexer->line, lexer->column);
                break;
            }            
            if (peek(lexer) == '\\') {
                switch (peek_next(lexer)) {
                    case 'n': buf[str_lit_len_bytes++] = '\n'; break;
                    case 't': buf[str_lit_len_bytes++] = '\t'; break;
                    case 'r': buf[str_lit_len_bytes++] = '\r'; break;
                    case '"': buf[str_lit_len_bytes++] = '"'; break;
                    case '\\': buf[str_lit_len_bytes++] = '\\'; break;
                    case '0': buf[str_lit_len_bytes++] = '\0'; break;
                    default: lex_error(lexer, "invalid escape sequence in string literal", lexer->line, lexer->column);
                }
                advance(lexer);
                if (peek(lexer) != '\n') advance(lexer);
            } else {
                buf[str_lit_len_bytes++] = advance(lexer);
            }
        } else {
            if (len == -1) {
                lex_error(lexer, "invalid UTF-8", lexer->line, lexer->column);
            } else {
                memcpy(buf + str_lit_len_bytes, lexer->src + lexer->pos, (size_t)len);
                advance_bytes_no_col(lexer, (size_t)(len - 1));
                str_lit_len_bytes += (size_t)len;
            }
            advance(lexer);
        }
        if (str_lit_len_bytes > MAX_STRING_LITERAL_LEN) {
            lex_error(lexer, "too long string literal", line_start, column_start);
            flush_until_quote_or_eol(lexer);
            break;
        }

    }
    char *lit = realloc(buf, str_lit_len_bytes + 1);
    if (!lit) { fprintf(stderr, "realloc"); exit(EXIT_FAILURE); }
    lit[str_lit_len_bytes] = '\0';
    
    ptrdiff_t l = lexer->src + lexer->pos - start_pos;
    Token str_token = make_token(TOK_STRING_LIT, start_pos, (size_t)l, line_start, column_start);
    set_str_lit_length_bytes(&str_token, str_lit_len_bytes + 1, lit);
    return str_token;
}

static void tokenvec_init(TokenVec *v) {
    v->data = malloc(128 * sizeof(Token));
    if (!v->data) {
        fprintf(stderr, "malloc\n");
        exit(EXIT_FAILURE);
    }
    v->cap = 128;
    v->count = 0;
}

static void tokenvec_push(TokenVec *v, Token t) {
    if (v->count == v->cap) {
        v->cap *= 2;
        v->data = realloc(v->data, v->cap * sizeof(Token));
        if (!v->data) {
            fprintf(stderr, "realloc\n");
            exit(EXIT_FAILURE);
        }
    }
    v->data[v->count++] = t;
}

static Token lexer_next(Lexer *const lexer) {
    skip_ignorable(lexer);
    char c = peek(lexer);

    if (c == '\0') return make_token(TOK_EOF, lexer->src + lexer->pos, 1, lexer->line, lexer->column);

    if (isalpha((unsigned char) c)) return scan_identifier(lexer);
    else if (isdigit((unsigned char) c)) return scan_number(lexer);
    else if (c == '"') return scan_string(lexer);

    const char *start_pos = lexer->src + lexer->pos;
    size_t line_start = lexer->line;
    size_t column_start = lexer->column;
    Token t;

    switch (*start_pos) {
        case '(': t = make_token(TOK_LPAREN, start_pos, 1, line_start, column_start); break;
        case ')': t = make_token(TOK_RPAREN, start_pos, 1, line_start, column_start); break;
        case '[': t = make_token(TOK_LBRACKET, start_pos, 1, line_start, column_start); break;
        case ']': t = make_token(TOK_RBRACKET, start_pos, 1, line_start, column_start); break;
        case '{': t = make_token(TOK_LBRACE, start_pos, 1, line_start, column_start); break;
        case '}': t = make_token(TOK_RBRACE, start_pos, 1, line_start, column_start); break;
        case '.': t = make_token(TOK_DOT, start_pos, 1, line_start, column_start); break;
        case ',': t = make_token(TOK_COMMA, start_pos, 1, line_start, column_start); break;
        case ':': t = make_token(TOK_COLON, start_pos, 1, line_start, column_start); break;
        case ';': t = make_token(TOK_SEMICOLON, start_pos, 1, line_start, column_start); break;
        case '+': t = make_token(TOK_PLUS, start_pos, 1, line_start, column_start); break;
        case '-': t = make_token(TOK_MINUS, start_pos, 1, line_start, column_start); break;
        case '*': t = make_token(TOK_STAR, start_pos, 1, line_start, column_start); break;
        case '/': t = make_token(TOK_SLASH, start_pos, 1, line_start, column_start); break;
        case '%': t = make_token(TOK_PERCENT, start_pos, 1, line_start, column_start); break;
        case '=':{
            if (peek_next(lexer) == '=') {
                t = make_token(TOK_EQ_EQ, start_pos, 2, line_start, column_start);
                advance(lexer);
            } else {
                t = make_token(TOK_EQ, start_pos, 1, line_start, column_start);
            }
            break;
        }
        case '<':{
            if (peek_next(lexer) == '=') {
                t = make_token(TOK_LT_EQ, start_pos, 2, line_start, column_start);
                advance(lexer);
            } else {
                t = make_token(TOK_LT, start_pos, 1, line_start, column_start);
            }
            break;
        }
        case '>':{
            if (peek_next(lexer) == '=') {
                t = make_token(TOK_GT_EQ, start_pos, 2, line_start, column_start);
                advance(lexer);
            }
            else {
                t = make_token(TOK_GT, start_pos, 1, line_start, column_start);
            }
            break;
        }
        case '!':{
            if (peek_next(lexer) == '=') {
                t = make_token(TOK_NOT_EQ, start_pos, 2, line_start, column_start);
                advance(lexer);
            } else {
                t = make_token(TOK_NOT, start_pos, 1, line_start, column_start);
            }
            break;
        }
        case '&':{
            if (peek_next(lexer) == '&') {
                t = make_token(TOK_AMP_AMP, start_pos, 2, line_start, column_start);
                advance(lexer);
            } else {
                lex_error(lexer, "unexpected '&'", line_start, column_start);
                t = make_token(TOK_UNKNOWN, start_pos, 1, line_start, column_start);
            }
            break;
        }
        case '|':{
            if (peek_next(lexer) == '|') {
                t = make_token(TOK_PIPE_PIPE, start_pos, 2, line_start, column_start);
                advance(lexer);
            } else {
                lex_error(lexer, "unexpected '|'", line_start, column_start);
                t = make_token(TOK_UNKNOWN, start_pos, 1, line_start, column_start);
            }
            break;
        }
        default: {
            if (lexer->utf8_error) break;
            
            lexer->utf8_error = true;
            int len = utf8_handler(lexer);
            if (len == 1) {
                lex_error(lexer, "unexpected character", line_start, column_start);
                t = make_token(TOK_UNKNOWN, start_pos, 1, line_start, column_start);
            } else if (len == -1) {
                lex_error(lexer, "invalid UTF-8", line_start, column_start);
                t = make_token(TOK_UNKNOWN, start_pos, 1, line_start, column_start);
            } else {
                lex_error(lexer, "non-ASCII character not allowed out of a string literal", line_start, column_start);
                advance_bytes_no_col(lexer, (size_t)(len - 1));
                t = make_token(TOK_UNKNOWN, start_pos, (size_t)len, line_start, column_start);
            }
        }
    }
    advance(lexer);
    return t;
}

static void lexer_init(Lexer *const lex, const char *s, size_t len, const char *path) {
    lex->src = s;
    lex->path = path;
    lex->length = len;
    lex->pos = 0;
    lex->line = 1;
    lex->column = 1;
    lex->errors = NULL;
    lex->err_count = 0;
    lex->err_cap = 0;
    lex->had_error = false;
    lex->utf8_error = false;
}

TokenVec lexer_all(const char *path) {
    size_t len = 0;
    char *src = read_file_or_exit(path, &len);
    Lexer lexer;
    lexer_init(&lexer, src, len, path);
    TokenVec vec; 
    tokenvec_init(&vec);

    for (size_t i = 0;; ++i) {
        if (i == MAX_TOKEN_AMOUNT) { lex_error(&lexer, "too many tokens", lexer.line, lexer.column); break; }
        Token tok = lexer_next(&lexer);
        tokenvec_push(&vec, tok);
        if (tok.type == TOK_EOF) break;
    }
    if (lexer.had_error) { dump_lex_errors(&lexer); exit(EXIT_FAILURE); }
    return vec;
}