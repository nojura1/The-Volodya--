#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "Token.h"
#include "TokenVec.h"

typedef enum {
    NK_NUMBER,
    NK_STRING,
    NK_BOOLEAN,
    NK_VAR,
    NK_UNARY,
    NK_BINARY,
    NK_CALL,
    NK_INDEX,
    NK_FIELD,
    NK_CAST
} NodeKind;

typedef struct {
    size_t pos;
    const char *file;
    size_t err_count;
    TokenVec *vec;
} ParserContext;

typedef struct Node Node;
typedef struct { Node **data; size_t length, cap; } NodeList;

struct Node {
    NodeKind kind;
    union {
        size_t boolean;
        struct { const char *start_pos; size_t length; } number;
        struct { const char *start_pos; size_t length; } string;
        struct { const char *start_pos; size_t length; } var;
        struct { Token op; Node *expr; } unary;
        struct { Token type; Node *expr; } cast;
        struct { Token op; Node *lhs; Node *rhs; } binary;
        struct { Node *called; NodeList args; } call;
        struct { Node *array; Node *index; } index_;
        struct { Node *strc; Node *var; } field;
    };
};

static inline Token peek(ParserContext *p) {
    return p->vec->data[p->pos];
}

static inline Token peek_next(ParserContext *p) {
    Token t = p->vec->data[p->pos];
    if (t.type == TOK_EOF) return t;
    return p->vec->data[p->pos + 1];
}

static inline Token consume(ParserContext *p) {
    Token t = p->vec->data[p->pos];
    if (t.type != TOK_EOF) p->pos++;
    return t;
}

static inline int match(ParserContext *p, TokenType tt) {
    if (peek(p).type == tt) { consume(p); return 1; }
    return 0;
}

static void error_tok(ParserContext *p, Token t, const char *msg) {
    fprintf(stderr, "%s:%zu:%zu: error: %s\n",
            p->file,
            t.line, t.column,
            msg
    );
    p->err_count++;
}

static void expect(ParserContext *p, TokenType expected) {
    Token t = peek(p);
    if (t.type == expected) { consume(p); return; }
    char buf[64];
    snprintf(buf, sizeof(buf), "expected token %d, saw %d", expected, t.type);
    error_tok(p, t, buf);
}

static void panic_until(ParserContext *p, TokenType a, TokenType b, TokenType c) {
    for (;;) {
        Token t = peek(p);
        if (t.type == TOK_EOF || t.type == a || t.type == b || t.type == c) return;
        p->pos++;
    }
}

static void push(NodeList *l, Node *n) {
    if (l->cap == l->length) {
        l->cap = l->cap ? l->cap * 2 : 4;
        l->data = realloc(l->data, sizeof(Node*) * l->cap);
        if (!l->data) { fprintf(stderr, "realloc\n"); exit(1); }
    }
    l->data[l->length++] = n;
}

static Node *mk(NodeKind k) {
    Node *n = calloc(1, sizeof(Node));
    if (!n) { fprintf(stderr, "calloc\n"); exit(1); }
    n->kind = k;
    return n;
}

static Node *mk_var(Token t) {
    Node *n = mk(NK_VAR);
    n->var.start_pos = t.start_pos;
    n->var.length = t.length;
    return n;
}

static Node *mk_string(Token t) {
    Node *n = mk(NK_STRING);
    n->string.start_pos = t.start_pos;
    n->string.length = t.length;
    return n;
}

static Node *mk_number(Token t) {
    Node *n = mk(NK_NUMBER);
    n->number.start_pos = t.start_pos;
    n->number.length = t.length;
    return n;
}

static Node *mk_boolean(size_t value) {
    Node *n = mk(NK_BOOLEAN);
    n->boolean = value;
    return n;
}

static Node *mk_unary(Token op, Node *expr) {
    Node *n = mk(NK_UNARY);
    n->unary.op = op;
    n->unary.expr = expr;
    return n;
}

static Node *mk_binary(Token t, Node *lhs, Node *rhs) {
    Node *n = mk(NK_BINARY);
    n->binary.op = t;
    n->binary.lhs = lhs;
    n->binary.rhs = rhs;
    return n;
}

static Node *mk_call(Node *called) {
    Node *n = mk(NK_CALL);
    n->call.called = called;
    return n;
}

static Node *mk_index(Node *array, Node *index) {
    Node *n = mk(NK_INDEX);
    n->index_.array = array;
    n->index_.index = index;
    return n;
}

static Node *mk_field(Node *strc, Token t) {
    Node *n = mk(NK_FIELD);
    n->field.strc = strc;
    n->field.var = mk_var(t);
    return n;
}

static Node *mk_cast(Token t, Node *expr) {
    Node *n = mk(NK_CAST);
    n->cast.type = t;
    n->cast.expr = expr;
    return n;
}

static int binary_op(TokenType type, size_t *lbp, size_t *rbp) {
    switch (type) {
        case TOK_STAR: case TOK_SLASH: case TOK_PERCENT: *lbp = 60; *rbp = 61; return 1;
        case TOK_PLUS: case TOK_MINUS: *lbp = 50; *rbp = 51; return 1;
        case TOK_LT: case TOK_LT_EQ: case TOK_GT: case TOK_GT_EQ: *lbp = 40; *rbp = 41; return 1;
        case TOK_EQ_EQ: case TOK_NOT_EQ: *lbp = 30; *rbp = 31; return 1;
        case TOK_AMP_AMP: *lbp = 20; *rbp = 21; return 1;
        case TOK_PIPE_PIPE: *lbp = 10; *rbp = 11; return 1;
        default: return 0;
    }
}

static Node* parse_expr(ParserContext *p, size_t min_bp);

static Node* parse_primary_postfix(ParserContext *p) {
    Token t = peek(p);
    Node *n = NULL;

    switch (t.type) {
        case TOK_IDENT: n = mk_var(consume(p)); break;
        case TOK_DEC_LIT: case TOK_HEX_LIT: case TOK_BIN_LIT: case TOK_OCT_LIT: n = mk_number(consume(p)); break;
        case TOK_STRING_LIT: n = mk_string(consume(p)); break;
        case TOK_KW_TRUE: n = mk_boolean(1); consume(p); break;
        case TOK_KW_FALSE: n = mk_boolean(0); consume(p); break;
        case TOK_LPAREN: consume(p); n = parse_expr(p, 0); expect(p, TOK_RPAREN); break;
        default:
            error_tok(p, t, "expected primary expression");
            consume(p);
            n = mk_number(t);
    }

    for (;;) {
        NodeList l = {NULL, 0, 0};
        Token t2 = peek(p);
        if (t2.type == TOK_LPAREN) {
            consume(p);
            n = mk_call(n);
            if (peek(p).type == TOK_RPAREN) { consume(p); n->call.args = l; continue; }
            for (;;) {
                push(&l, parse_expr(p, 0));
                if (!match(p, TOK_COMMA)) break;
            }
            n->call.args = l;
            expect(p, TOK_RPAREN);
        } else if (t2.type == TOK_LBRACKET) {
            consume(p);
            n = mk_index(n, parse_expr(p, 0));
            expect(p, TOK_RBRACKET);
        } else if (t2.type == TOK_DOT && peek_next(p).type == TOK_IDENT) {
            consume(p);
            n = mk_field(n, consume(p));
        } else break;
    }

    return n;
}

static inline int is_unary(Token t) {
    return t.type == TOK_MINUS || t.type == TOK_NOT || t.type == TOK_PLUS;
}

static inline int try_parse_cast(ParserContext *p) {
    size_t tmp = p->pos;
    if (consume(p).type != TOK_LPAREN) { p->pos = tmp; return 0; }
    Token t = peek(p);

    if (t.type == TOK_KW_I32 || t.type == TOK_KW_I64 || t.type == TOK_KW_U32 ||
        t.type == TOK_KW_U64 || t.type == TOK_KW_BOOL || t.type == TOK_KW_STRING) {
        consume(p);
    } else if (t.type == TOK_IDENT) {
        consume(p);
        for (; peek(p).type == TOK_DOT && peek_next(p).type == TOK_IDENT;) {
            consume(p); consume(p);
        }
    }

    for (; peek(p).type == TOK_LBRACKET && peek_next(p).type == TOK_RBRACKET;) {
        consume(p); consume(p);
    }

    if (match(p, TOK_RBRACE)) return 1;
    else { p->pos = tmp; return 0; }
}

static Node* parse_expr(ParserContext *p, size_t min_bp) {
    Node *left = NULL;

    if (min_bp < 70 && try_parse_cast(p)) {
        
        Node *n = parse_expr(p, 70);
        //left = mk_cast(t, n);
    } else if (is_unary(peek(p))) {
        Token op = consume(p);
        Node *n = parse_expr(p, 80);
        left = mk_unary(op, n);
    } else 
        left = parse_primary_postfix(p);

    for (;;) {
        Token op = peek(p);
        size_t lbp, rbp;
        if (!binary_op(op.type, &lbp, &rbp)) break;
        if (lbp < min_bp) break;

        consume(p);
        Node *right = parse_expr(p, rbp);
        left = mk_binary(op, left, right);
    }

    return left;
}

void dump_parser(size_t d, Node *n) {
    for (int i = 0; i < d; i++) printf(" ");
    if (!n) { printf("(null)\n"); return; }
    switch (n->kind) {
        case NK_NUMBER: printf("Number(%.*s)", (int)n->number.length, n->number.start_pos); break;
        case NK_BOOLEAN: printf("Boolean(%d)", n->boolean ? 1 : 0); break;
        case NK_VAR: printf("Var(%.*s)", (int)n->var.length, n->var.start_pos); break;
        case NK_STRING: printf("String(%.*s)", (int)n->string.length, n->string.start_pos); break;
        case NK_CALL:
            printf("call (");
            dump_parser(0, n->call.called);
            printf(", [");
            for (int i = 0; i < n->call.args.length; i++) {
                dump_parser(0, n->call.args.data[i]);
                i == n->call.args.length - 1 ? : printf(", ");
            }
            printf("])"); break;
        case NK_INDEX:
            printf("index (");
            dump_parser(0, n->index_.array);
            printf(", ");
            dump_parser(0, n->index_.index);
            printf(")"); break;
        case NK_FIELD:
            printf("field (");
            dump_parser(0, n->field.strc);
            printf(", ");
            dump_parser(0, n->field.var);
            printf(")"); break;
        case NK_BINARY:
            printf("binary ('%.*s'\n", (int)n->binary.op.length, n->binary.op.start_pos);
            dump_parser(d + 1, n->binary.lhs);
            printf("\n");
            dump_parser(d + 1, n->binary.rhs);
            printf("\n)"); break;
        case NK_UNARY:
            printf("unary ('%.*s'\n", (int)n->unary.op.length, n->unary.op.start_pos);
            dump_parser(d + 1, n->unary.expr);
            printf("\n)");
        case NK_CAST:
            printf("cast ('%.*s'\n", (int)n->cast.type.length, n->cast.type.start_pos);
            dump_parser(d + 1, n->cast.expr);
            printf("\n)");
        default: return;
    }
}

void parse(TokenVec vec) {
    ParserContext content = (ParserContext){0, "test.vol", 0, &vec};
    Node *root = parse_expr(&content, 0);
    dump_parser(0, root);
    printf("\n");
}