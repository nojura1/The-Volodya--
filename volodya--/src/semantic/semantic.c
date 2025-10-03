#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include "../parser/parser.h"
#include "../lexer/lexer.h"

const char *path = NULL;

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
    T_FN
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

static void error_pr(size_t line, size_t column, const char *msg) {
    fprintf(stderr, "%s:%zu:%zu: semantic error: %s\n", path, line, column, msg);
}

static int type_eq(const Type *a, const Type *b) {
    if (a == b) return 1;
    if (!a || !b) return 0;
    if (a->kind != b->kind) return 0;

    switch (a->kind) {
        case T_VOLOID: case T_BOOL: case T_I32: case T_I64: case T_U32: case T_U64: case T_STRING: return 1;
        case T_STRUCT: return a->struct_.length == b->struct_.length && !strncmp(a->struct_.start_pos, b->struct_.start_pos, a->struct_.length);
        case T_ARRAY: return a->array.length == b->array.length && type_eq(a->array.elem, b->array.elem);
        case T_FN:
            if (a->fn.nparams != b->fn.nparams || !type_eq(a->fn.ret, b->fn.ret)) return 0;
            for (size_t i = 0; i < a->fn.nparams; i++) {
                if (!type_eq(a->fn.params[i], b->fn.params[i])) return 0;
            }
            return 1;
        default: return 0;
    }
}

static inline int is_int(const Type *t) { return t->kind == T_I32 || t->kind == T_I64 || t->kind == T_U32 || t->kind == T_U64; }
static inline int is_bool(const Type *t) { return t->kind == T_BOOL; }
static inline int is_voloid(const Type *t) { return t->kind == T_VOLOID; }
static inline int is_array(const Type *t) { return t->kind == T_ARRAY; }

static int can_cast_explicit(const Type *dst, const Type *src) {
    return is_int(src) && is_int(dst);
}

static Type *mk_type(TypeKind kind) {
    Type *t = calloc(1, sizeof(Type));
    if (!t) { fprintf(stderr, "calloc\n"); exit(1); }
    t->kind = kind;
    return t;
}

static Type *mk_type_array(Type *elem, int32_t length) {
    Type *t = mk_type(T_ARRAY);
    t->array.elem = elem;
    t->array.length = length;
    return t;
}

static Type *mk_type_fn(Type **params, size_t nparams, Type *ret) {
    Type *t = mk_type(T_FN);
    t->fn.params = params;
    t->fn.nparams = nparams;
    t->fn.ret = ret;
    return t;
}

static Type *mk_type_struct(const char *start_pos, size_t length) {
    Type *t = mk_type(T_STRUCT);
    t->struct_.start_pos = start_pos;
    t->struct_.length = length;
    return t;
}

static Symbol *mk_symbol(SymKind kind) {
    Symbol *sym = calloc(1, sizeof(Symbol));
    if (!sym) { fprintf(stderr, "calloc\n"); exit(1); }
    sym->kind = kind;
    return sym;
}

static Symbol *mk_symbol_const(size_t column, size_t line, Type *type, const char *start_pos, size_t length) {
    Symbol *sym = mk_symbol(SYM_CONST);
    sym->type = type;
    sym->column = column;
    sym->line= line;
    sym->start_pos = start_pos;
    sym->length = length;
    return sym;
}

static Symbol *mk_symbol_struct(size_t column, size_t line, Type *type, const char *start_pos, size_t length) {
    Symbol *sym = mk_symbol(SYM_STRUCT);
    sym->type = type;
    sym->column = column;
    sym->line= line;
    sym->start_pos = start_pos;
    sym->length = length;
    return sym;
}

static Symbol *mk_symbol_var(size_t column, size_t line, Type *type, const char *start_pos, size_t length, int is_initialized) {
    Symbol *sym = mk_symbol(SYM_VAR);
    sym->type = type;
    sym->column = column;
    sym->line= line;
    sym->start_pos = start_pos;
    sym->length = length;
    sym->is_initialized = is_initialized;
    return sym;
}

static Symbol *mk_symbol_fn(size_t column, size_t line, Type *type, const char *start_pos, size_t length, FnState fn_state) {
    Symbol *sym = mk_symbol(SYM_FN);
    sym->type = type;
    sym->column = column;
    sym->line= line;
    sym->start_pos = start_pos;
    sym->length = length;
    sym->fn_state = fn_state;
    return sym;
}

static inline Scope *scope_push(Scope *parent) {
    Scope *s = calloc(1, sizeof(Scope));
    s->parent = parent;
    return s;
}

static inline Scope *scope_pop(Scope *s) {
    for (size_t i = 0; i < s->length; i++) free(s->arr[i]);
    Scope *tmp = s->parent;
    free(s);
    return tmp;
}

static Symbol *sym_lookup(Scope *s, const char *start_pos, size_t length) {
    for (size_t i = 0; i < s->length; i++) {
        if (s->arr[i]->length == length && !strncmp(s->arr[i]->start_pos, start_pos, length)) return s->arr[i];
    }
    if (!s->parent) return NULL;
    return sym_lookup(s->parent, start_pos, length);
}

static void sym_insert(Scope *s, Symbol *new_sym) {
    if (s->length == s->cap) {
        s->cap = s->cap ? s->cap * 2 : 8;
        s->arr = realloc(s->arr, s->cap * sizeof(Symbol *));
        if (!s->arr) { fprintf(stderr, "realloc"); exit(1); }
    }

    Symbol *old_sym = sym_lookup(s, new_sym->start_pos, new_sym->length);
    
    if (!old_sym) s->arr[s->length++] = new_sym;
    else if (new_sym->kind == SYM_FN && old_sym->kind == SYM_FN) {
        if (old_sym->fn_state == FN_DECLARED && new_sym->fn_state == FN_DECLARED
            && type_eq(old_sym->type, new_sym->type)) {
            s->arr[s->length++] = new_sym;
        } else if (old_sym->fn_state == FN_DECLARED && new_sym->fn_state == FN_DEFINED
            && type_eq(old_sym->type, new_sym->type)) {
            old_sym->fn_state = FN_DEFINED;
            s->arr[s->length++] = new_sym;
        } else if (old_sym->fn_state == FN_DEFINED) {
            error_pr(new_sym->line, new_sym->column, "function is already defined");
        } else {
            error_pr(new_sym->line, new_sym->column, "function name is already taken");
        }
    } else
        error_pr(new_sym->line, new_sym->column, "var is already defined");
}

static inline Type *mk_type_from_kw(TokenType tok) {
    Type *t;
    switch (tok) {
        case TOK_KW_I32: t = mk_type(T_I32); break;
        case TOK_KW_I64: t = mk_type(T_I64); break;
        case TOK_KW_U32: t = mk_type(T_U32); break;
        case TOK_KW_U64: t = mk_type(T_U64); break;
        case TOK_KW_BOOL: t = mk_type(T_BOOL); break;
        case TOK_KW_STRING: t = mk_type(T_STRING); break;
        case TOK_KW_VOLOID: t = mk_type(T_VOLOID); break;
        default: t = NULL;
    }
    return t;
}

static Type *type_res(Node *node) {
    switch (node->kind) {
        case NK_VAR: return mk_type_from_kw(node->var.type);
        case NK_INDEX: return mk_type_array(type_res(node->index_.array), (int32_t)node->index_.index->number.value);
        default: return NULL;
    }
}

static Type **mk_param_t_list(Node *node) {
    NodeList params = node->fn.params;
    Type **type_list = malloc(sizeof(Type *) * params.length);
    for (size_t i = 0; i < params.length; ++i) {
        type_list[i] = type_res(params.data[i]);
    }
    return type_list;
}

static void name_res(Node *node, Scope *s) {
    if (!node) return;
    switch (node->kind) {
        case NK_PROGRAM:
            for (size_t i = 0; i < node->program.length; i++) { 
                name_res(node->program.data[i], s); 
            }
            break;
        case NK_FN:{
            Type *ret = type_res(node->fn.ret_type);
            Type **param_list = mk_param_t_list(node);
            size_t n_params = node->fn.params.length;
            Type *fn = mk_type_fn(param_list, n_params, ret);

            Symbol *sym = mk_symbol_fn(
                node->fn.name.column,
                node->fn.name.line,
                fn,
                node->fn.name.start_pos,
                node->fn.name.length,
                !node->fn.stmt ? FN_DECLARED : FN_DEFINED
            );
            
            if (is_array(ret) && is_voloid(ret->array.elem))
                error_pr(sym->line, sym->column, "voloid[] as function return type is not allowed");

            sym_insert(s, sym);
            Scope *new_s = scope_push(s);
            name_res(node->fn.stmt, new_s);
            scope_pop(new_s);
            break;
        }
        case NK_BLOCK: for (size_t i = 0; i < node->stmts.length; i++) name_res(node->stmts.data[i], s); break;
        case NK_IF:{
            Scope *new_s = scope_push(s);
            name_res(node->if_stmt.cond, new_s);
            name_res(node->if_stmt.stmt, new_s);
            scope_pop(new_s);
            
            Scope *else_s = scope_push(s);
            name_res(node->if_stmt.else_chain, else_s);
            scope_pop(else_s);
            break;
        }
        case NK_WHILE:{
            Scope *new_s = scope_push(s);
            name_res(node->while_loop.cond, new_s);
            name_res(node->while_loop.stmt, new_s);
            scope_pop(new_s);
            break;
        }
        case NK_FOR:{
            Scope* new_s = scope_push(s);
            for (size_t i = 0; i < node->for_stmt.init.length; i++) name_res(node->for_stmt.init.data[i], new_s);
            name_res(node->for_stmt.expr, new_s);
            for (size_t i = 0; i < node->for_stmt.step.length; i++) name_res(node->for_stmt.step.data[i], new_s);
            name_res(node->for_stmt.stmt, new_s);
            scope_pop(new_s);
            break;
        }
        case NK_DECLEXPR:{
            size_t column = node->declexpr.name.column, line = node->declexpr.name.line;
            Type *t = type_res(node->declexpr.type);
            const char *start_pos = node->declexpr.name.start_pos;
            size_t length = node->declexpr.name.length;  

            Symbol *sym = node->declexpr.is_const ? mk_symbol_const(column, line, t, start_pos, length)
                : mk_symbol_var(column, line, t, start_pos, length, !!node->declexpr.rvalue);

            if (!node->declexpr.rvalue && sym->kind == SYM_CONST)
                error_pr(line, column, "cosnt variable must be initialized at declaration");

            sym_insert(s, sym);
            break;
        }
        case NK_STRUCT:{
            Token struct_name = node->struct_.name;
            Symbol *sym = mk_symbol_struct(
                struct_name.column,
                struct_name.line,
                mk_type_struct(struct_name.start_pos, struct_name.length),
                struct_name.start_pos,
                struct_name.length
            );

            sym_insert(s, sym);
            Scope *new_s = scope_push(s);
            for (size_t i = 0; i < node->struct_.fields.length; i++) {
                name_res(node->struct_.fields.data[i], new_s);
            }
            scope_pop(new_s);
            break;
        }
        case NK_STC_FIELD:{
            Token name = node->stc_field.name;
            Type *t = type_res(node->stc_field.type);

            Symbol *sym = mk_symbol_var(name.column, name.line, t, name.start_pos,
                name.length, node->declexpr.rvalue != NULL);

            sym_insert(s, sym);
            break;
        }
        default: return;
    }
} 

int main(void) {
    path = "../tests/test.vol";
    Node *n = parse(lexer_all(path), path);
    Scope *s = calloc(1, sizeof(Scope));
    //printf("%d\n", n->program.data[0]->kind);
    
    name_res(n, s);

    return EXIT_SUCCESS;
}