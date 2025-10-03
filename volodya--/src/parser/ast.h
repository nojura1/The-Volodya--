#pragma once

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
    NK_CAST,
    NK_ARRAY_LIT,
    NK_BREAK,
    NK_IF,
    NK_WHILE,
    NK_FOR,
    NK_RETURN,
    NK_EXPR,
    NK_BLOCK,
    NK_ASSIGN,
    NK_DECLEXPR,
    NK_FN,
    NK_PARAM,
    NK_STRUCT,
    NK_STC_FIELD,
    NK_PROGRAM,
    NK_ERROR
} NodeKind;

typedef struct {
    size_t array_depth;
    size_t start_pos;
    size_t end_pos;
} TokenCast;

typedef struct Node Node;
typedef struct { Node **data; size_t length, cap; } NodeList;

struct Node {
    NodeKind kind;
    union {
        const char *error;
        struct { size_t value; Token t; } boolean;
        struct { long long value; Token t; } number;
        Token string;
        Token var;
        NodeList array_lit;
        struct { Token op; Node *expr; } unary;
        struct { Node *cast_type; Node *expr; } cast;
        struct { Token op; Node *lhs; Node *rhs; } binary;
        struct { Node *called; NodeList args; } call;
        struct { Node *array; Node *index; } index_;
        struct { Node *strc; Node *var; } field;
        struct { Node *cond; Node *stmt; } while_loop;
        Node *return_stmt;
        struct { Node *cond; Node *stmt; Node *else_chain; } if_stmt;
        struct { bool is_const; Node *type; Token name; Node *rvalue; } declexpr;
        struct { Node *lvalue; Node *rvalue; } assign;
        Node *expr_stmt;
        NodeList stmts;
        struct { NodeList init; Node *expr; NodeList step; Node *stmt; } for_stmt;
        struct { Node *ret_type; Token name; NodeList params; Node *stmt; } fn;
        struct { Node *p_type; Token name; } param;
        struct { Token name; NodeList fields; } struct_;
        struct { Token name; Node *type; } stc_field;
        NodeList program;
    };
};