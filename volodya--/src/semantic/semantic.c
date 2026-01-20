#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include "semantic.h"
#include "../parser/parser.h"
#include "../lexer/lexer.h"
#include <stdarg.h>
#include <limits.h>

#define SEM_ERR_LIMIT 10
#define SEM_ERR_MSG_MAX 256

typedef struct {
    size_t line, column;
    const char *start_pos;
    size_t length;
    char msg[SEM_ERR_MSG_MAX];
} SemErr;

typedef struct {
    const char *start_pos;
    size_t length;
    Type *type;
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

typedef struct {
    StructTable table;
    const char *path;
    Scope *root;
    Type *current_ret;
    SemErr *errs;
    size_t nerrs, cap;

    size_t total_reports;
    size_t dropped_dupes;
    size_t dropped_limit;
} SCtx;

typedef struct {
    size_t len, cap;
    Symbol **init_symbols;
} SetInit;

static int semerr_cmp_key(const SemErr *a, const SemErr *b) {
    if (a->line < b->line) return -1;
    if (a->line > b->line) return 1;
    if (a->column < b->column) return -1;
    if (a->column > b->column) return 1;
    return 0;
}

static bool semerr_equal(const SemErr *a, const SemErr *b) {
    return a->line == b->line && a->column == b->column;
}

static void sem_error(SCtx *ctx, size_t line, size_t column, const char *start_pos, size_t length, const char *fmt, ...) {
    if (!ctx) return;
    ctx->total_reports++;

    SemErr cand;
    cand.line = line;
    cand.column = column;
    cand.start_pos = start_pos;
    cand.length = length;

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(cand.msg, SEM_ERR_MSG_MAX, fmt, ap);
    va_end(ap);

    for (size_t i = 0; i < ctx->nerrs; ++i) {
        if (semerr_equal(&ctx->errs[i], &cand)) {
            ctx->dropped_dupes++;
            return;
        }
    }

    if (ctx->nerrs >= SEM_ERR_LIMIT) {
        ctx->dropped_limit++;
        return;
    }

    if (ctx->nerrs == ctx->cap) {
        ctx->cap = ctx->cap ? ctx->cap * 2 : 8;
        ctx->errs = realloc(ctx->errs, ctx->cap * sizeof(SemErr));
        if (!ctx->errs) { fprintf(stderr, "realloc\n"); exit(EXIT_FAILURE); }
    }

    size_t pos = 0;
    while (pos < ctx->nerrs && semerr_cmp_key(&ctx->errs[pos], &cand) <= 0) {
        pos++;
    }

    if (pos < ctx->nerrs) {
        memmove(&ctx->errs[pos + 1], &ctx->errs[pos], (ctx->nerrs - pos) * sizeof(SemErr));
    }

    ctx->errs[pos] = cand;
    ctx->nerrs++;
}

static void sem_flush_errors(SCtx *ctx) {
    if (!ctx || ctx->nerrs == 0) return;

    for (size_t i = 0; i < ctx->nerrs; ++i) {
        SemErr *e = &ctx->errs[i];
        fprintf(stderr, "%s:%zu:%zu: semantic error: %s",
                ctx->path ? ctx->path : "<input>",
                e->line, e->column, e->msg);

        if (e->start_pos && e->length) {
            fprintf(stderr, " ('%.*s')", (int)e->length, e->start_pos);
        }
        fprintf(stderr, "\n");
    }

    if (ctx->dropped_dupes) {
        fprintf(stderr, "%s: note: %zu duplicate error(s) suppressed.\n",
                ctx->path ? ctx->path : "<input>", ctx->dropped_dupes);
    }
    if (ctx->dropped_limit) {
        fprintf(stderr, "%s: too many errors. Showing first %zu unique.\n",
                ctx->path ? ctx->path : "<input>", ctx->nerrs);
    }
}

static void sem_free(SCtx *ctx) {
    if (!ctx) return;
    free(ctx->errs);
    ctx->errs = NULL;
    ctx->nerrs = ctx->cap = 0;
}

static inline bool sem_too_many(const SCtx *ctx) {
    return ctx && ctx->nerrs >= SEM_ERR_LIMIT;
}

static int type_eq(const Type *a, const Type *b) {
    if (a == b) return 1;
    if (!a || !b) return 0;
    if (a->kind != b->kind) return 0;

    switch (a->kind) {
        case T_VOLOID: case T_BOOL: case T_I32: case T_I64: case T_U32: case T_U64: case T_STRING: return 1;
        case T_STRUCT: return a->struct_.length == b->struct_.length && !strncmp(a->struct_.start_pos, b->struct_.start_pos, a->struct_.length);
        case T_ARRAY: return type_eq(a->array.elem, b->array.elem);
        case T_FN:
            if (a->fn.nparams != b->fn.nparams || !type_eq(a->fn.ret, b->fn.ret)) return 0;
            for (size_t i = 0; i < a->fn.nparams; ++i) {
                if (!type_eq(a->fn.params[i], b->fn.params[i])) return 0;
            }
            return 1;
        default: return 0;
    }
}

static inline int is_int(const Type *t) { return t->kind == T_I32 || t->kind == T_I64 || t->kind == T_U32 || t->kind == T_U64; }
static inline int is_signed_int(const Type *t) { return t->kind == T_I32 || t->kind == T_I64; }
static inline int is_bool(const Type *t) { return t->kind == T_BOOL; }
static inline int is_voloid(const Type *t) { return t->kind == T_VOLOID; }
static inline int is_array(const Type *t) { return t->kind == T_ARRAY; }
static inline int is_struct(const Type *t) { return t->kind == T_STRUCT; }

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
    Scope *tmp = s->parent;
    free(s->arr);
    return tmp;
}

static Symbol *sym_lookup(Scope *s, const char *start_pos, size_t length) {
    for (size_t i = 0; i < s->length; ++i) {
        if (s->arr[i]->length == length && !strncmp(s->arr[i]->start_pos, start_pos, length)) return s->arr[i];
    }
    if (!s->parent) return NULL;
    return sym_lookup(s->parent, start_pos, length);
}

static void sym_insert(Scope *s, Symbol *new_sym, SCtx *ctx) {
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
            sem_error(ctx, new_sym->line, new_sym->column,
                NULL, 0, 
                "function '%.*s' is already defined", (int)new_sym->length, new_sym->start_pos);
        } else {
            sem_error(ctx, new_sym->line, new_sym->column,
                new_sym->start_pos, new_sym->length,
                "function name is already taken");
        }
    } else
        sem_error(ctx, new_sym->line, new_sym->column,
            NULL, 0,
            "'%.*s' is already defined", (int)new_sym->length, new_sym->start_pos);
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

static Type *type_res(Node *node, Scope *s, SCtx *ctx) {
    switch (node->kind) {
        case NK_T_BUILTIN: return mk_type_from_kw(node->t_builtin.type);
        case NK_T_IDENT:
            Symbol *sym = sym_lookup(s, node->t_ident.start_pos, node->t_ident.length);
            if (sym && sym->kind == SYM_STRUCT)
                return mk_type_struct(node->t_ident.start_pos, node->t_ident.length);
            else
                sem_error(ctx, node->line, node->column,
                NULL, 0, "'%.*s' is of unknown type", (int)node->t_ident.length, node->t_ident.start_pos);
            
            return NULL;
        case NK_T_ARRAY:{
            return mk_type_array(type_res(node->t_array.type, s, ctx), -1);
        }
        default: return NULL;
    }
}

static Type **mk_param_t_list(Node *node, Scope *s, SCtx *ctx) {
    NodeList params = node->fn.params;
    Type **type_list = malloc(sizeof(Type *) * params.length);
    for (size_t i = 0; i < params.length; ++i) {
        type_list[i] = type_res(params.data[i]->param.p_type, s, ctx);
    }
    return type_list;
}

static void add_structdef(StructTable *table, StructDef *def) {
    if (table->cap == table->length) {
        table->cap = table->cap ? table->cap * 2 : 8;
        table->table = realloc(table->table, table->cap * sizeof(StructDef *));
        if (!table->table) { fprintf(stderr, "realloc"); exit(EXIT_FAILURE); }
    }
    table->table[table->length++] = def;
}

static StructField *mk_struct_field(const char *start_pos, size_t length, Type *t) {
    StructField *f = malloc(sizeof(StructField));
    f->start_pos = start_pos;
    f->length = length;
    f->type = t;
    return f;
}

static StructDef *mk_struct_def(const char *start_pos, size_t length, StructField **fields, size_t nfields) {
    StructDef *d = malloc(sizeof(StructDef));
    d->start_pos = start_pos;
    d->length = length;
    d->fields = fields;
    d->nfields = nfields;
    return d;
}

static StructField *field_lookup(size_t index, StructField **f, const char *start_pos, size_t length) {
    for (size_t i = 0; i < index; ++i)
        if (f[i]->length == length && !strncmp(f[i]->start_pos, start_pos, length)) return f[i];
    return NULL;
}

static StructDef *def_lookup(SCtx *ctx, const char *start_pos, size_t length) {
    for (size_t i = 0; i < ctx->table.length; ++i)
        if (ctx->table.table[i]->length == length &&
        !strncmp(ctx->table.table[i]->start_pos, start_pos, length)) return ctx->table.table[i];
    return NULL;
}

static void resolve_struct(Node *node, Scope *s, SCtx *ctx) {
    size_t nf = node->struct_.fields.length;
    StructField **fields = malloc(sizeof(StructField *) * nf);

    for (size_t i = 0; i < nf; ++i) {
        Token name = node->struct_.fields.data[i]->stc_field.name;
        Type *t = type_res(node->struct_.fields.data[i]->stc_field.type, s, ctx);
        node->struct_.fields.data[i]->type = t;

        if (is_struct(t) && (t->struct_.length == node->struct_.name.length &&
        !strncmp(t->struct_.start_pos, node->struct_.name.start_pos, t->struct_.length))) {
            sem_error(ctx, name.line, name.column, NULL, 0,
                "recursive field '%.*s' must be a pointer, but pointers are not supported", (int)name.length, name.start_pos);
                fields[i] = NULL;
                continue;
        }
        StructField *f = field_lookup(i, fields, name.start_pos, name.length);
        if (!f) {
            fields[i] = mk_struct_field(name.start_pos, name.length, t);
        } else {
            sem_error(ctx, name.line, name.column, NULL, 0, "field '%.*s' already defined", (int)name.length, name.start_pos);
            fields[i] = NULL;
        }
    }
    StructDef *d = def_lookup(ctx, node->struct_.name.start_pos, node->struct_.name.length);
    
    if (!d)
        add_structdef(&ctx->table, mk_struct_def(node->struct_.name.start_pos, node->struct_.name.length, fields, nf));
}

static void name_res(Node *node, Scope *s, SCtx *ctx) {
    if (sem_too_many(ctx)) return;
    if (!node) return;
    
    switch (node->kind) {
        case NK_PROGRAM:{
            for (size_t i = 0; i < node->program.length; ++i) { 
                name_res(node->program.data[i], s, ctx); 
            }
            break;
        }
        case NK_FN:{
            Type *ret = type_res(node->fn.ret_type, s, ctx);
            Type **param_list = mk_param_t_list(node, s, ctx);
            size_t nparams = node->fn.params.length;
            Type *fn = mk_type_fn(param_list, nparams, ret);
            node->type = fn;

            Symbol *sym = mk_symbol_fn(
                node->fn.name.column,
                node->fn.name.line,
                fn,
                node->fn.name.start_pos,
                node->fn.name.length,
                !node->fn.stmt ? FN_DECLARED : FN_DEFINED
            );
            
            if (is_array(ret) && is_voloid(ret->array.elem)) {
                sem_error(ctx, sym->line, sym->column, NULL, 0, "voloid[] as function return type is not allowed");
                break;
            }
            sym_insert(s, sym, ctx);
            Scope *new_s = scope_push(s);
            for (size_t i = 0; i < nparams; ++i) {
                name_res(node->fn.params.data[i], new_s, ctx);
            }
            name_res(node->fn.stmt, new_s, ctx);
            scope_pop(new_s);
            break;
        }
        case NK_PARAM:{
            Token name = node->param.name;
            name_res(node->param.p_type, s, ctx);
            Type *t = type_res(node->param.p_type, s, ctx);
            node->type = t;

            Symbol *sym = mk_symbol_var(name.column, name.line, t, name.start_pos, name.length, 0);
            sym_insert(s, sym, ctx);
            node->param.sym = sym;
            break;
        }
        case NK_BLOCK:{
            Scope *new_s = scope_push(s);
            for (size_t i = 0; i < node->stmts.length; ++i) name_res(node->stmts.data[i], new_s, ctx);
            scope_pop(new_s);
            break;
        }
        case NK_IF:{
            Scope *new_s = scope_push(s);
            name_res(node->if_stmt.cond, new_s, ctx);
            name_res(node->if_stmt.stmt, new_s, ctx);
            scope_pop(new_s);
            
            Scope *else_s = scope_push(s);
            name_res(node->if_stmt.else_chain, else_s, ctx);
            scope_pop(else_s);
            break;
        }
        case NK_WHILE:{
            Scope *new_s = scope_push(s);
            name_res(node->while_loop.cond, new_s, ctx);
            name_res(node->while_loop.stmt, new_s, ctx);
            scope_pop(new_s);
            break;
        }
        case NK_FOR:{
            Scope* new_s = scope_push(s);
            for (size_t i = 0; i < node->for_stmt.init.length; ++i) name_res(node->for_stmt.init.data[i], new_s, ctx);
            name_res(node->for_stmt.expr, new_s, ctx);
            for (size_t i = 0; i < node->for_stmt.step.length; ++i) name_res(node->for_stmt.step.data[i], new_s, ctx);
            name_res(node->for_stmt.stmt, new_s, ctx);
            scope_pop(new_s);
            break;
        }
        case NK_DECLEXPR:{
            size_t column = node->column, line = node->line;
            const char *start_pos = node->declexpr.name.start_pos;
            size_t length = node->declexpr.name.length;
            
            name_res(node->declexpr.type, s, ctx);
            Type *t = type_res(node->declexpr.type, s, ctx);
            node->type = t;

            Symbol *sym = node->declexpr.is_const ? mk_symbol_const(column, line, t, start_pos, length)
                : mk_symbol_var(column, line, t, start_pos, length, !!node->declexpr.rvalue);            

            if (!node->declexpr.rvalue && sym->kind == SYM_CONST)
                sem_error(ctx, line, column, NULL, 0,
                    "cosntant variable '%.*s' must be initialized at declaration", (int)length, start_pos);
            
            if (node->declexpr.rvalue) name_res(node->declexpr.rvalue, s, ctx);
            sym_insert(s, sym, ctx);
            node->declexpr.sym = sym;
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

            sym_insert(s, sym, ctx);
            resolve_struct(node, s, ctx);
            break;
        }
        case NK_ASSIGN:{
            name_res(node->assign.lvalue, s, ctx);
            name_res(node->assign.rvalue, s, ctx);
            break;
        }
        case NK_RETURN:{
            name_res(node->return_stmt, s, ctx);
            break;
        }
        case NK_EXPR:{
            name_res(node->expr_stmt, s, ctx);
            break;    
        }
        case NK_ARRAY_LIT:{
            for (size_t i = 0; i < node->array_lit.elems.length; ++i) {
                name_res(node->array_lit.elems.data[i], s, ctx);
            }
            break;
        }
        case NK_FIELD:{
            name_res(node->field.strc, s, ctx);
            break;
        }
        case NK_T_ARRAY:{
            name_res(node->t_array.length, s, ctx);
            break;
        }
        case NK_BINARY:{
            name_res(node->binary.lhs, s, ctx);
            name_res(node->binary.rhs, s, ctx);
            break;
        }
        case NK_UNARY:{
            name_res(node->unary.expr, s, ctx);
            break;
        }
        case NK_CALL:{
            name_res(node->call.called, s, ctx);
            for (size_t i = 0; i < node->call.args.length; ++i)
                name_res(node->call.args.data[i], s, ctx);

            Node *callee = node->call.called;

            if (callee->kind == NK_IDENT) {
                Symbol *sym = callee->ident_.resolved;
                if (!sym || sym->kind != SYM_FN) {
                    sem_error(ctx, callee->line, callee->column, NULL, 0, "called expression is not a function");
                } else {
                    node->call.resolved_fn = sym;
                }
            } else if (callee->kind == NK_FIELD) {
                sem_error(ctx, node->line, node->column, NULL, 0,
                    "method calls not supported (a.b(...))");
            } else {
                sem_error(ctx, node->line, node->column, NULL, 0,
                    "cannot call a non-function expression");
            }
            break;
        }
        case NK_INDEX:{
            name_res(node->index_.index, s, ctx);
            name_res(node->index_.array, s, ctx);
            break;
        }
        case NK_IDENT:{
            Token t = node->ident_.ident;
            Symbol *sym = sym_lookup(s, t.start_pos, t.length);
            
            if (!sym) {
                sem_error(ctx, t.line, t.column, NULL, 0, "identifier expression '%.*s' is not defined", (int)t.length, t.start_pos);
            } else if (sym->kind == SYM_STRUCT) {
                sem_error(ctx, t.line, t.column, NULL, 0, "type '%.*s' cannot be treated as an identifier", (int)t.length, t.start_pos);
            } else {
                node->ident_.resolved = sym;
            }
            break;
        }
        default: return;
    }
}

static inline Type *ann(Node *n, Type *t, bool is_lval) { n->type = t; n->is_lvalue = is_lval; return t; }

static const char *type_kind[T__COUNT] = {"voloid", "bool", "i32", "i64", "u32", "u64", "string", "array", "struct", "fn"};

static Type *check_expr(Node *expr, SCtx *ctx) {
    if (sem_too_many(ctx)) return NULL;
    if (!expr) return NULL;
    
    switch (expr->kind) {
        case NK_BINARY:{
            Type *lhs = check_expr(expr->binary.lhs, ctx);
            Type *rhs = check_expr(expr->binary.rhs, ctx);
            if (!lhs || !rhs) return ann(expr, NULL, false);
            
            Token t = expr->binary.op;
            bool is_both_bool = is_bool(lhs) && is_bool(rhs);
            bool is_both_int = is_int(lhs) && is_int(rhs);

            switch (t.type) {
                case TOK_EQ_EQ: case TOK_NOT_EQ: case TOK_GT: case TOK_LT: case TOK_GT_EQ: case TOK_LT_EQ:
                    if (is_both_bool || (is_both_int && type_eq(lhs, rhs))) return ann(expr, mk_type(T_BOOL), false);
                    break;
                case TOK_AMP_AMP: case TOK_PIPE_PIPE:
                    if (is_both_bool) return ann(expr, mk_type(T_BOOL), false);
                    break;
                default: {
                    if (is_both_int && type_eq(lhs, rhs)) return ann(expr, lhs, false);
                    break;
                }
            }
            if (is_both_int && !type_eq(lhs, rhs)) {
                sem_error(ctx, expr->line, expr->column, NULL, 0, "expected same numeric types for %.*s (have '%s' and '%s')",
                    (int)expr->binary.op.length, expr->binary.op.start_pos, type_kind[lhs->kind],  type_kind[rhs->kind]);
            } else {
                sem_error(ctx, expr->line, expr->column, NULL, 0, "invalid operands to %.*s (have '%s' and '%s')",
                    (int)expr->binary.op.length, expr->binary.op.start_pos, type_kind[lhs->kind],  type_kind[rhs->kind]);
            }
            return ann(expr, NULL, false);
        }
        case NK_CAST:{
            Type *cast_expr = check_expr(expr->cast.expr, ctx);
            if (!cast_expr) return ann(expr, NULL, false);

            Node *t = expr->cast.cast_type;
            Type *cast_type = NULL;
            if (t->kind == NK_T_BUILTIN) {
                cast_type = mk_type_from_kw(t->t_builtin.type);
            } else {
                sem_error(ctx, t->line, t->column, NULL, 0, "only builtin types are allowed in casts");
                return ann(expr, NULL, false);
            }
            if (!can_cast_explicit(cast_type, cast_expr)) {
                sem_error(ctx, expr->line, expr->column, NULL, 0, "cannot cast from %s to %s",
                    type_kind[cast_expr->kind], type_kind[cast_type->kind]);
                return ann(expr, NULL, false);
            }
            return ann(expr, cast_type, false);
        }
        case NK_UNARY:{
            Type *t = check_expr(expr->unary.expr, ctx);
            if (!t) return ann(expr, NULL, false);
            Token tok = expr->unary.op;

            if ((tok.type == TOK_MINUS || tok.type == TOK_PLUS) && !is_signed_int(t)) {
                sem_error(ctx, expr->line, expr->column, NULL, 0, "invalid operand to %c (have '%s')", *(tok.start_pos), type_kind[t->kind]);
                return ann(expr, NULL, false);
            } else if (tok.type == TOK_NOT && !is_bool(t)) {
                sem_error(ctx, expr->line, expr->column, NULL, 0, "invalid operand to %c (have '%s')", *(tok.start_pos), type_kind[t->kind]);
                return ann(expr, NULL, false);
            }
            return ann(expr, t, false);
        }
        case NK_INDEX:{
            Type *ta = check_expr(expr->index_.array, ctx);
            Type *ti = check_expr(expr->index_.index, ctx);
            if (!ti || !ta) return ann(expr, NULL, false);

            if (!is_array(ta)) {
                sem_error(ctx, expr->index_.array->line, expr->index_.array->column, NULL, 0, "indexing non-array");
                return ann(expr, NULL, false);
            }
            if (ti->kind != T_I32) {
                sem_error(ctx, expr->index_.index->line, expr->index_.index->column, NULL, 0,
                    "type 'i32' is expected, but '%s' was provided", type_kind[ti->kind]);
                return ann(expr, NULL, false);
            }
            return ann(expr, ta->array.elem, expr->index_.array->is_lvalue);
        }
        case NK_CALL:{
            Symbol *sym = expr->call.resolved_fn;
            if (!sym) return ann(expr, NULL, false);

            if (expr->call.args.length != sym->type->fn.nparams) {
                sem_error(ctx, expr->call.called->line, expr->call.called->column, NULL, 0,
                    "funcion '%.*s' expectes %zu arguments, but %zu were provided",
                    (int)sym->length, sym->start_pos, sym->type->fn.nparams, expr->call.args.length);

                return ann(expr, NULL, false);
            } else {
                for (size_t i = 0; i < expr->call.args.length; ++i) {
                    Type *t = check_expr(expr->call.args.data[i], ctx);
                    if (!t) return ann(expr, NULL, false);

                    if (!type_eq(t, sym->type->fn.params[i])) {
                        sem_error(ctx, expr->call.args.data[i]->line, expr->call.args.data[i]->column, NULL, 0,
                            "funcion '%.*s' expectes parameter of type '%s', but type '%s' was provided",
                            (int)sym->length, sym->start_pos, type_kind[sym->type->fn.params[i]->kind], type_kind[expr->call.args.data[i]->type->kind]);
                        return ann(expr, NULL, false);
                    }
                }
            }
            return ann(expr, sym->type->fn.ret, false);   
        }
        case NK_FIELD:{
            Type *t = check_expr(expr->field.strc, ctx);
            if (!t) return ann(expr, NULL, false);

            if (!is_struct(t)) {
                sem_error(ctx, expr->line, expr->column, NULL, 0, "left of '.' is not a struct");
                return ann(expr, NULL, false);
            }
            StructDef *d = def_lookup(ctx, t->struct_.start_pos, t->struct_.length);
            if (!d) return ann(expr, NULL, false); 
            
            Token tok = expr->field.ident->ident_.ident;
            StructField *f = field_lookup(d->nfields, d->fields, tok.start_pos, tok.length);
            
            if (!f) {
                sem_error(ctx, tok.line, tok.column, d->start_pos, d->length, 
                    "'%.*s' is not a field of a struct", (int)tok.length, tok.start_pos);
                return ann(expr, NULL, false);    
            }
            return ann(expr, f->type, expr->field.strc->is_lvalue);
        }
        case NK_ARRAY_LIT:{
            size_t len = expr->array_lit.elems.length;
            NodeList elems = expr->array_lit.elems;
            if (len == 0) {
                sem_error(ctx, expr->line, expr->column, NULL, 0, "array literal consists of 0 elemets)");
                return ann(expr, NULL, false);
            }
            
            Type *elem_type = check_expr(elems.data[0], ctx);
            if (!elem_type) return ann(expr, NULL, false);
            
            for (size_t i = 1; i < len; ++i) {
                Type *t = check_expr(elems.data[i], ctx);
                if (!t) return ann(expr, NULL, false);

                if (!type_eq(elem_type, t)) {
                    sem_error(ctx, expr->line, expr->column, NULL, 0, 
                        "array literal consists of different types (position %zu is of type '%s' but expected '%s')",
                        i + 1, type_kind[t->kind], type_kind[elem_type->kind]);
                    return ann(expr, NULL, false);
                }
            }
            Type *arr_type = mk_type_array(elem_type, (int32_t)len);
            return ann(expr, arr_type, false);
        }
        case NK_IDENT:{
            Symbol *sym = expr->ident_.resolved;
            if (!sym) return ann(expr, NULL, false);
            return ann(expr, sym->type, sym->kind == SYM_VAR);
        }
        case NK_NUMBER:{
            unsigned long long v = (unsigned long long)expr->number.value;
            if (v <= INT_MAX || v == 0x8000'0000)
                return ann(expr, mk_type(T_I32), false);
            else if (v <= LLONG_MAX || v == 0x8000'0000'0000'0000)
                return ann(expr, mk_type(T_I64), false);
            
            return ann(expr, mk_type(T_U64), false);
        }
        case NK_BOOLEAN: return ann(expr, mk_type(T_BOOL), false);
        case NK_STRING: return ann(expr, mk_type(T_STRING), false);
        default: return NULL;
    }
}

static bool falls_through(Node *stmt);

static bool can_break(Node *stmt) {
    if (!stmt) return false;

    switch (stmt->kind) {
        case NK_IF:{
            int is_cond = -1;
            if (stmt->if_stmt.cond->kind == NK_BOOLEAN) is_cond = stmt->if_stmt.cond->boolean.value;
            
            switch (is_cond) {
                case 1: return can_break(stmt->if_stmt.stmt);
                case 0:
                    if (!stmt->if_stmt.else_chain) return false;
                    else return can_break(stmt->if_stmt.else_chain);
                case -1:
                    if (can_break(stmt->if_stmt.stmt) || can_break(stmt->if_stmt.else_chain)) return true;
                    return false;
            }
        }
        case NK_BLOCK:{
            for (size_t i = 0; i < stmt->stmts.length; ++i) {
                if (can_break(stmt->stmts.data[i])) return true;
                if (!falls_through(stmt->stmts.data[i])) break;
            }
            return false;
        }
        case NK_BREAK: return true;
        case NK_RETURN: case NK_FOR: case NK_WHILE: return false;
        default: return false;
    }
}

static bool falls_through(Node *stmt) {
    if (!stmt) return true;

    switch (stmt->kind) {
        case NK_FOR:{
            int is_cond = -1;
            if (!stmt->for_stmt.expr) is_cond = 1;
            else if (stmt->for_stmt.expr->kind == NK_BOOLEAN) is_cond = stmt->for_stmt.expr->boolean.value;

            if (is_cond == 0 || is_cond == -1) return true;
            return can_break(stmt->for_stmt.stmt);
        }
        case NK_WHILE:{
            int is_cond = -1;
            if (stmt->while_loop.cond->kind == NK_BOOLEAN) is_cond = stmt->while_loop.cond->boolean.value;

            if (is_cond == 0 || is_cond == -1) return true;
            return can_break(stmt->while_loop.stmt);
        }
        case NK_IF:{
            int is_cond = -1;
            if (stmt->if_stmt.cond->kind == NK_BOOLEAN) is_cond = stmt->if_stmt.cond->boolean.value;

            switch (is_cond) {
                case 1: return falls_through(stmt->if_stmt.stmt);
                case 0:
                    if (!stmt->if_stmt.else_chain) return true;
                    else return falls_through(stmt->if_stmt.else_chain);
                case -1:
                    if (!falls_through(stmt->if_stmt.stmt) && !falls_through(stmt->if_stmt.else_chain)) return false;
                    return true;
            }
        }
        case NK_BLOCK:{
            for (size_t i = 0; i < stmt->stmts.length; ++i) { 
                if (!falls_through(stmt->stmts.data[i])) return false;
            }
            return true;
        }
        case NK_BREAK: case NK_RETURN: return false; 
        default: return true;
    }
}

static void check_stmt(Node *node, SCtx *ctx, int loop_depth) {
    if (sem_too_many(ctx)) return;
    if (!node) return;

    switch (node->kind) {
        case NK_WHILE:{
            Type *t = check_expr(node->while_loop.cond, ctx);

            if (t && !is_bool(t)) {
                sem_error(ctx, node->while_loop.cond->line, node->while_loop.cond->column, NULL, 0,
                    "condition must be of type 'bool' (have '%s')", type_kind[t->kind]);
            }
            check_stmt(node->while_loop.stmt, ctx, loop_depth + 1);
            break;
        }
        case NK_BREAK:{
            if (!loop_depth) sem_error(ctx, node->line, node->column, NULL, 0, "break outside of loop");
            break;
        }
        case NK_RETURN:{
            Type *fn_ret = ctx->current_ret;
        
            if (!node->return_stmt) {
                if (fn_ret && !is_voloid(fn_ret)) {
                    sem_error(ctx, node->line, node->column, NULL, 0, "non-voloid function must return a value");
                }
            } else {
                Type *t = check_expr(node->return_stmt, ctx);
                if (fn_ret && is_voloid(fn_ret)) {
                    sem_error(ctx, node->line, node->column, NULL, 0, "voloid function cannot return a value");
                } else if (fn_ret && t && !type_eq(t, fn_ret)) {
                    sem_error(ctx, node->line, node->column, NULL, 0,
                        "return type mismatch (have '%s', expected '%s')", type_kind[t->kind], type_kind[fn_ret->kind]);
                }
            }
            break;
        }
        case NK_IF:{
            Type *t = check_expr(node->if_stmt.cond, ctx);

            if (t && !is_bool(t)) {
                sem_error(ctx, node->if_stmt.cond->line, node->if_stmt.cond->column, NULL, 0,
                    "condition must be of type 'bool' (have '%s')", type_kind[t->kind]);
            }
            check_stmt(node->if_stmt.stmt, ctx, loop_depth);
            check_stmt(node->if_stmt.else_chain, ctx, loop_depth);
            break;
        }
        case NK_DECLEXPR: {
            if (node->type && node->type->kind == T_ARRAY) {
                Node *len = node->declexpr.type->t_array.length;
                Type *tl = check_expr(len, ctx);
                if (tl && tl->kind != T_I32) {
                    sem_error(ctx, len->line, len->column, NULL, 0,
                        "array length must be of type 'i32' (have '%s')", type_kind[tl->kind]);
                }
            }
            Type *t = check_expr(node->declexpr.rvalue, ctx);
            if (t && node->type && !type_eq(t, node->type)) {
                if (is_array(t) && is_array(node->type)) {
                    sem_error(ctx, node->line, node->column, NULL, 0,
                        "incompatible types in initialization (have array of type '%s', expected array of type '%s')",
                        type_kind[t->array.elem->kind], type_kind[node->type->array.elem->kind]);
                } else if (is_struct(t)) {
                    sem_error(ctx, node->line, node->column, NULL, 0,
                        "incompatible types in initialization (have struct of type '%.*s', expected '%s')",
                        (int)t->struct_.length, t->struct_.start_pos, type_kind[node->type->kind]);
                } else if (is_struct(node->type)) {
                    sem_error(ctx, node->line, node->column, NULL, 0,
                        "incompatible types in initialization (have '%s', expected struct of type '%.*s')",
                        type_kind[t->kind], (int)node->type->struct_.length, node->type->struct_.start_pos);
                } else {
                    sem_error(ctx, node->line, node->column, NULL, 0,
                        "incompatible types in initialization (have '%s', expected '%s')", type_kind[t->kind], type_kind[node->type->kind]);
                }
            }
            break;
        }
        case NK_ASSIGN:{
            Type *lv = check_expr(node->assign.lvalue, ctx);
            Type *rv = check_expr(node->assign.rvalue, ctx);
            if (!lv || !rv) return;
            
            if (!node->assign.lvalue->is_lvalue) {
                sem_error(ctx, node->line, node->column, NULL, 0, "cannot assign to non-lvalue");
                break;
            }
            if (!type_eq(lv, rv)) {
                if (is_array(lv) && is_array(rv)) {
                    sem_error(ctx, node->line, node->column, NULL, 0,
                        "incompatible types in initialization (have array of type '%s', expected array of type '%s')",
                        type_kind[lv->array.elem->kind], type_kind[rv->array.elem->kind]);
                } else if (is_struct(lv)) {
                    sem_error(ctx, node->line, node->column, NULL, 0,
                        "incompatible types in initialization (have '%s', expected struct of type '%.*s')",
                        type_kind[rv->kind], (int)lv->struct_.length, lv->struct_.start_pos);
                } else if (is_struct(rv)) {
                    sem_error(ctx, node->line, node->column, NULL, 0,
                        "incompatible types in initialization (have struct of type '%.*s', expected '%s')",
                        (int)rv->struct_.length, rv->struct_.start_pos, type_kind[lv->kind]);
                } else {
                sem_error(ctx, node->line, node->column, NULL, 0,
                    "incompatible types in assignment (have '%s', expected '%s')", type_kind[rv->kind], type_kind[lv->kind]);
                }
            }
            break;
        }
        case NK_EXPR:{
            check_expr(node->expr_stmt, ctx);
            break;    
        }
        case NK_BLOCK:{
            for (size_t i = 0; i < node->stmts.length; ++i) { 
                check_stmt(node->stmts.data[i], ctx, loop_depth);
            }
            break;
        }
        case NK_FOR:{
            for (size_t i = 0; i < node->for_stmt.init.length; i++) check_stmt(node->for_stmt.init.data[i], ctx, loop_depth);
            
            Type *t = check_expr(node->for_stmt.expr, ctx);

            if (t && !is_bool(t)) {
                sem_error(ctx, node->for_stmt.expr->line, node->for_stmt.expr->column, NULL, 0,
                    "condition must be of type 'bool' (have '%s')", type_kind[t->kind]);
            }
            for (size_t i = 0; i < node->for_stmt.step.length; i++) check_stmt(node->for_stmt.step.data[i], ctx, loop_depth);
            
            check_stmt(node->for_stmt.stmt, ctx, loop_depth + 1);
            break;
        }
        case NK_FN: {
            ctx->current_ret = node->type->fn.ret;
            check_stmt(node->fn.stmt, ctx, loop_depth);
            
            if (node->fn.stmt && !is_voloid(node->type->fn.ret) && falls_through(node->fn.stmt)) { 
                Token name = node->fn.name;
                sem_error(ctx, name.line, name.column, name.start_pos, name.length, "control may reach end of non-voloid function");
            }
            break;
        }
        case NK_STRUCT:{
            for (size_t i = 0; i < node->struct_.fields.length; ++i) {
                check_stmt(node->struct_.fields.data[i], ctx, loop_depth);
            }
            break;
        }
        case NK_STC_FIELD:{
            if (node->type && node->type->kind == T_ARRAY) {
                Node *len = node->stc_field.type->t_array.length;
                Type *tl = check_expr(len, ctx);
                if (tl && tl->kind != T_I32) {
                    sem_error(ctx, len->line, len->column, NULL, 0,
                        "array length must be of type 'i32' (have '%s')", type_kind[tl->kind]);
                }
            }
            break;
        }
        case NK_PROGRAM:{
            for (size_t i = 0; i < node->program.length; ++i) { 
                check_stmt(node->program.data[i], ctx, loop_depth); 
            }
            break;
        }
        default: return;
    }
}

typedef enum {
    READ,
    ADDR 
} Mode;

static void free_set(SetInit *s) { free(s->init_symbols); free(s); }

static bool has(SetInit *set, Symbol *sym) {
    for (size_t i = 0; i < set->len; ++i) {
        Symbol *s = set->init_symbols[i];
        if (s == sym) return true;
    }
    return false;
}

static void add(SetInit *set, Symbol *sym) {
    if (has(set, sym)) return;

    if (set->len == set->cap) {
        set->cap = set->cap ? set->cap * 2 : 8;
        set->init_symbols = realloc(set->init_symbols, set->cap * sizeof(Symbol *));
        if (!set->init_symbols) { fprintf(stderr, "realloc\n"); exit(EXIT_FAILURE); }
    }
    set->init_symbols[set->len++] = sym;
}

static SetInit *clone(SetInit *set) {
    SetInit *clone = malloc(sizeof(SetInit));
    if (!clone) { fprintf(stderr, "malloc\n"); exit(EXIT_FAILURE); }
    
    clone->len = set->len;
    clone->cap = set->cap;
    clone->init_symbols = NULL;

    if (clone->cap) {
        clone->init_symbols = malloc(clone->cap * sizeof(Symbol *));
        if (!clone->init_symbols) { fprintf(stderr, "malloc\n"); exit(EXIT_FAILURE); }
        memcpy(clone->init_symbols, set->init_symbols, clone->len * sizeof(Symbol *));
    }
    return clone;
}

static SetInit *intersect(SetInit *set1, SetInit *set2) {
    SetInit *new = calloc(1, sizeof(SetInit));
    if (!new) { fprintf(stderr, "calloc\n"); exit(EXIT_FAILURE); }
    if (!(set1->len && set2->len)) return new;
    
    SetInit *smaller = set1->len > set2->len ? set2 : set1;
    SetInit *larger = set1->len > set2->len ? set1 : set2;
    
    for (size_t i = 0; i < smaller->len; ++i) {
        if (has(larger, smaller->init_symbols[i])) add(new, smaller->init_symbols[i]);
    }
    return new;
}

static Symbol *root_value(Node *expr) {
    if (!expr) return NULL;

    switch (expr->kind) {
        case NK_IDENT: return expr->ident_.resolved;
        case NK_FIELD: return root_value(expr->field.strc);
        case NK_INDEX: return root_value(expr->index_.array);
        default: return NULL;
    }
}

static bool check_init_expr(Node *e, SCtx *ctx, SetInit *set, Mode mode) {
    if (sem_too_many(ctx)) return false;
    if (!e) return false;

    switch (e->kind) {
        case NK_IDENT:{
            if (mode == ADDR) return true;

            Symbol *sym = root_value(e);
            if (!sym) return false;

            if (!has(set, sym)) {
                sem_error(ctx, e->line, e->column, e->ident_.ident.start_pos, e->ident_.ident.length,
                    "use of uninitialized variable");
                    return false;
            }
            return true;
        }
        case NK_CALL:{
            for (size_t i = 0; i < e->call.args.length; ++i) {
                if (!check_init_expr(e->call.args.data[i], ctx, set, mode)) {
                    return false;
                }
            }
            return true;
        }
        case NK_ARRAY_LIT:{
            for (size_t i = 0; i < e->array_lit.elems.length; ++i) {
                if (!check_init_expr(e->array_lit.elems.data[i], ctx, set, READ)) {
                    return false;
                }
            }
            return true;
        }
        case NK_INDEX: return check_init_expr(e->index_.index, ctx, set, READ) && check_init_expr(e->index_.array, ctx, set, mode);
        case NK_BINARY: return check_init_expr(e->binary.lhs, ctx, set, mode) && check_init_expr(e->binary.rhs, ctx, set, mode);
        case NK_UNARY: return check_init_expr(e->unary.expr, ctx, set, mode);
        case NK_FIELD: return check_init_expr(e->field.strc, ctx, set, mode);
        case NK_NUMBER: case NK_BOOLEAN:
        case NK_STRING: default: return true;
    }
}

static void init_check_stmt(SCtx *ctx, Node *stmt, SetInit *set, int in_loop) {
    if (sem_too_many(ctx)) return;
    if (!stmt) return;

    switch (stmt->kind) {
        case NK_WHILE:{
            check_init_expr(stmt->while_loop.cond, ctx, set, READ);
            SetInit *clone1 = clone(set);
            init_check_stmt(ctx, stmt->while_loop.stmt, clone1, in_loop + 1);
            free_set(clone1);
            break;
        }
        case NK_IF:{
            check_init_expr(stmt->if_stmt.cond, ctx, set, READ);
            SetInit *clone1 = clone(set);

            init_check_stmt(ctx, stmt->if_stmt.stmt, clone1, in_loop);
            SetInit *clone2 = clone(set);

            init_check_stmt(ctx, stmt->if_stmt.else_chain, clone2, in_loop);
            SetInit *out = intersect(clone1, clone2);

            free(set->init_symbols);
            *set = *out;
            
            free(out);
            free_set(clone1);
            free_set(clone2);
            break;
        }
        case NK_DECLEXPR: {
            if (!stmt->declexpr.rvalue) return;

            check_init_expr(stmt->declexpr.rvalue, ctx, set, READ);
            add(set, stmt->declexpr.sym);
            break;
        }
        case NK_ASSIGN:{
            check_init_expr(stmt->assign.rvalue, ctx, set, READ);
            check_init_expr(stmt->assign.lvalue, ctx, set, ADDR);
            Symbol *sym = root_value(stmt->assign.lvalue);

            if (sym)
                add(set, sym);
            break;
        }
        case NK_BLOCK:{
            for (size_t i = 0; i < stmt->stmts.length; ++i) { 
                init_check_stmt(ctx, stmt->stmts.data[i], set, in_loop);
            }
            break;
        }
        case NK_FOR:{
            for (size_t i = 0; i < stmt->for_stmt.init.length; ++i) init_check_stmt(ctx, stmt->for_stmt.init.data[i], set, in_loop);
            SetInit *clone1 = clone(set);
            check_init_expr(stmt->for_stmt.expr, ctx, clone1, READ);
            init_check_stmt(ctx, stmt->for_stmt.stmt, clone1, in_loop + 1);
            for (size_t i = 0; i < stmt->for_stmt.step.length; ++i) init_check_stmt(ctx, stmt->for_stmt.step.data[i], clone1, in_loop);
            free_set(clone1);
            break;
        }
        case NK_RETURN: check_init_expr(stmt->return_stmt, ctx, set, READ); break;
        case NK_EXPR: check_init_expr(stmt->expr_stmt, ctx, set, READ); break;
        default: return;
    }
}

static void init_check_fn(SCtx *ctx, Node *fn) {
    SetInit *set = calloc(1, sizeof(SetInit));
    Node **params = fn->fn.params.data;

    for (size_t i = 0; i < fn->fn.params.length; ++i) {
        add(set, params[i]->param.sym);
    }
    init_check_stmt(ctx, fn->fn.stmt, set, 0);
}

static void init_check_all(SCtx *ctx, Node *root) {
    for (size_t i = 0; i < root->program.length; ++i) {
        if (root->program.data[i]->kind == NK_FN) init_check_fn(ctx, root->program.data[i]);
    }
}

void sema(Node *root, const char *path) {
    SCtx ctx = {0};
    ctx.path = path;
    Scope *global = calloc(1, sizeof(Scope));

    name_res(root, global, &ctx);
    check_stmt(root, &ctx, 0);
    init_check_all(&ctx, root);
    sem_flush_errors(&ctx);

    if (ctx.nerrs) exit(EXIT_FAILURE);
}

int main(void) {
    const char *path = "../tests/test.vol";

    Node *n = parse(lexer_all(path), path);
    sema(n, path);

    return EXIT_SUCCESS;
}