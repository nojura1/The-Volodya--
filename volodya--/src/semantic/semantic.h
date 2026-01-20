#pragma once

typedef enum {
    T_VOLOID,
    T_BOOL,
    T_I32,
    T_I64,
    T_U32,
    T_U64,
    T_STRING,
    T_ARRAY,
    T_STRUCT,
    T_FN,
    T__COUNT
} TypeKind;

typedef enum { SYM_VAR, SYM_CONST, SYM_FN, SYM_STRUCT } SymKind;
typedef enum { FN_DECLARED, FN_DEFINED } FnState;

typedef struct Type Type;
typedef struct Scope Scope;

struct Type { 
    TypeKind kind;
    union {
        struct { const char *start_pos; size_t length; } struct_;
        struct { Type *elem; int32_t length; } array;
        struct { Type **params; size_t nparams; Type *ret; } fn;
    };
};

typedef struct {
    SymKind kind;
    const char *start_pos;
    size_t length;
    size_t column, line;
    Type *type;
    union {
        FnState fn_state;
        int is_initialized;
    };
} Symbol;

struct Scope {
    Symbol **arr;
    size_t length, cap;
    Scope *parent;
};