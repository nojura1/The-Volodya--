#pragma once
#include <stdint.h>

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

typedef enum {
    BUILTIN_NONE,
    BUILTIN_PRINT
} BuiltInKind;

typedef enum { SYM_VAR, SYM_CONST, SYM_FN, SYM_STRUCT } SymKind;
typedef enum { FN_DECLARED, FN_DEFINED } FnState;

typedef struct Node Node;
typedef struct Type Type;
typedef struct Scope Scope;

typedef struct {
    const char *start_pos;
    size_t length;
    Type *type;
    Node *field_decl;
} StructField;

typedef struct {
    const char *start_pos;
    size_t length;
    StructField **fields;
    size_t nfields;
} StructDef;

typedef struct {
    size_t length, cap;
    StructDef **table;
} StructTable;

struct Type { 
    TypeKind kind;
    union {
        struct { int32_t length_bytes; } string;
        struct { const char *start_pos; size_t length; Node *struct_decl; } struct_;
        struct { Type *elem; int32_t length; } array;
        struct { Type **params; size_t nparams; Type *ret; } fn;
    };
};

typedef struct {
    SymKind kind;
    const char *start_pos;
    size_t length;
    size_t column, line;
    size_t stack_offset;
    Type *type;
    BuiltInKind bi_kind;
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

void sema(Node *root, const char *path);