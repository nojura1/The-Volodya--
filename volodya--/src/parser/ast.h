#pragma once
#include "../semantic/semantic.h"

typedef enum {
    NK_NUMBER,
    NK_STRING_LIT,
    NK_BOOLEAN,
    NK_T_BUILTIN,
    NK_T_IDENT,
    NK_IDENT,
    NK_UNARY,
    NK_BINARY,
    NK_CALL,
    NK_INDEX,
    NK_T_ARRAY,
    NK_FIELD,
    NK_CAST,
    NK_ARRAY_LIT,
    NK_T_STRING,
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

typedef struct Node Node;
typedef struct { Node **data; size_t length, cap; } NodeList;

struct Node {
    NodeKind kind;
    bool is_lvalue;
    Type *type;
    size_t column, line;
    union {
        const char *error;
        struct { size_t value; Token t; } boolean;
        struct { unsigned long long value; Token t; } number;
        struct { Token value; } string_lit;
        struct { Token ident; Symbol *resolved; } ident_;
        struct { Node *length; } t_string;
        Token t_builtin;
        Token t_ident;
        struct { NodeList elems; } array_lit;
        struct { Token op; Node *expr; } unary;
        struct { Node *cast_type; Node *expr; } cast;
        struct { Token op; Node *lhs; Node *rhs; } binary;
        struct { Node *called; NodeList args; Symbol *resolved_fn; } call;
        struct { Node *array; Node *index; } index_;
        struct { Node *type; Node *length; } t_array;
        struct { Node *strc; Node *ident; StructField *field_decl; } field;
        struct { Node *cond; Node *stmt; } while_loop;
        Node *return_stmt;
        struct { Node *cond; Node *stmt; Node *else_chain; } if_stmt;
        struct { bool is_const; Node *type; Token name; Node *rvalue; Symbol *sym; } declexpr;
        struct { Node *lvalue; Node *rvalue; } assign;
        Node *expr_stmt;
        NodeList stmts;
        struct { NodeList init; Node *expr; NodeList step; Node *stmt; } for_stmt;
        struct { Node *ret_type; Token name; NodeList params; Node *stmt; } fn;
        struct { Node *p_type; Token name; Symbol *sym; } param;
        struct { Token name; NodeList fields; size_t size, align; } struct_;
        struct { Token name; Node *type; size_t field_offset, size; } stc_field;
        NodeList program;
    };
};