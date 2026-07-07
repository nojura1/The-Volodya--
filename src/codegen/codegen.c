#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "codegen.h"
#include "../Token/TokenNames.h"
#include "../parser/parser.h"

typedef struct {
    size_t top;
    char label[256][64];
} LabelStack;

typedef struct {
    FILE *out;
    LabelStack labels;
    int next_label;
} CodegenCtx;

typedef enum {
    SCALAR,
    AGGREGATE
} CallArgKind;

typedef struct CallArgPlan CallArgPlan;

struct CallArgPlan {
    Node *arg;
    CallArgKind kind;
    size_t size, align;
    size_t offset;
    CallArgPlan *storage_plan;
};

typedef struct {
    bool is_builtin_print;
    size_t outgoing_stack_size;
    size_t arg_count;
    CallArgPlan args[256];
} CallPlan;

static inline int is_signed_int(const Type *t) { return t->kind == T_I32 || t->kind == T_I64; }

static size_t size_of(Type *t) {
    if (!t) return 0;

    switch (t->kind) {
        case T_BOOL:
        case T_I32:
        case T_U32:
            return 4;

        case T_I64:
        case T_U64:
            return 8;

        case T_STRING:
            return (size_t)t->string.length_bytes;

        case T_STRUCT:
            return t->struct_.struct_decl->struct_.size;

        case T_ARRAY:{
            return size_of(t->array.elem) * (size_t)t->array.length;
        }

        default:
            return 8;
    }
}

static size_t align_of(Type *t) {
    if (!t) return 0;

    switch (t->kind) {
        case T_BOOL:
        case T_I32:
        case T_U32:
            return 4;

        case T_I64:
        case T_U64:
            return 8;

        case T_STRING:
            return 1;

        case T_STRUCT:
            return t->struct_.struct_decl->struct_.align;

        case T_ARRAY:
            return align_of(t->array.elem);

        default:
            return 8;
    }
}

static size_t pa_size_of(Type *t) {
    if (!t) return 0;

    switch (t->kind) {
        case T_BOOL:
        case T_I32:
        case T_U32:
            return 4;

        case T_I64:
        case T_U64:
        case T_ARRAY:
        case T_STRING:
            return 8;

        case T_STRUCT:
            return t->struct_.struct_decl->struct_.size;

        default:
            return 8;
    }
}

static size_t pa_align_of(Type *t) {
    if (!t) return 0;

    switch (t->kind) {
        case T_BOOL:
        case T_I32:
        case T_U32:
            return 4;

        case T_I64:
        case T_U64:
        case T_ARRAY:
        case T_STRING:
            return 8;

        case T_STRUCT:
            return t->struct_.struct_decl->struct_.align;

        default:
            return 8;
    }
}

static size_t align_up(size_t n, size_t a) {
    size_t remainder = n % a;
    return (remainder) ? n + a - remainder : n;
}

static void emit(CodegenCtx *ctx, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(ctx->out, fmt, ap);
    va_end(ap);
}

static void emit_copy_bytes(CodegenCtx *ctx, const char *dst_reg, const char *src_reg, size_t size) {
    long sz = (long)size;
    for (long off = 0; sz > 0;) {
        if (sz >= 8) {
            emit(ctx, "\tmov r10, qword [%s + %ld]\n", src_reg, off);
            emit(ctx, "\tmov qword [%s + %ld], r10\n", dst_reg, off);
            off += 8;
            sz -= 8;
        } else if (sz >= 4) {
            emit(ctx, "\tmov r10d, dword [%s + %ld]\n", src_reg, off);
            emit(ctx, "\tmov dword [%s + %ld], r10d\n", dst_reg, off);
            off += 4;
            sz -= 4;
        } else {
            emit(ctx, "\tmov r10b, byte [%s + %ld]\n", src_reg, off);
            emit(ctx, "\tmov byte [%s + %ld], r10b\n", dst_reg, off);
            off++;
            sz--;
        }
    }
}

static bool is_scalar(Type *t) { return t->kind == T_I32 || t->kind == T_I64 || t->kind == T_U32 || t->kind == T_U64 || t->kind == T_BOOL || t->kind == T_VOLOID; }
static bool is_aggregate(Type *t) { return t->kind == T_ARRAY || t->kind == T_STRUCT || t->kind == T_STRING; }

static bool is_incomplete_array(Type *t) { return t->kind == T_ARRAY && t->array.length == -1; }
static bool is_incomplete_string(Type *t) { return t->kind == T_STRING && t->string.length_bytes == -1; }

static void gen_expr(CodegenCtx *ctx, Node *expr);
static void gen_aggregate_expr_into(CodegenCtx *ctx, Node *expr);

static void gen_addr(CodegenCtx *ctx, Node *expr) {
    if (!expr) return;

    switch (expr->kind) {
        case NK_IDENT:{
            Symbol *sym = expr->ident_.resolved;
            if (is_incomplete_array(sym->type) || is_incomplete_string(sym->type)) emit(ctx, "\tmov rax, [rbp - %zu]\n", sym->stack_offset);
            else emit(ctx, "\tlea rax, [rbp - %zu]\n", sym->stack_offset);
            break;
        }
        case NK_FIELD:{
            gen_addr(ctx, expr->field.strc);
            Node *field = expr->field.field_decl->field_decl;
            emit(ctx, "\tadd rax, %zu\n", field->stc_field.field_offset);
            break;
        }
        case NK_INDEX:{
            gen_addr(ctx, expr->index_.array);
            emit(ctx, "\tmov r8, rax\n");
            gen_expr(ctx, expr->index_.index);
            
            Type *t = expr->index_.array->type;
            size_t elem_size = (t->kind == T_STRING) ? 1 : size_of(expr->type);

            emit(ctx, "\tmovsxd rax, eax\n");
            emit(ctx, "\timul rax, %zu\n", elem_size);
            emit(ctx, "\tadd rax, r8\n");
            break;
        }
        default: return;
    }
}

static void eval_storage_for_temp_args(CallArgPlan *arg_plan, size_t *cur) {
    Node *arg = arg_plan->arg;
    CallArgPlan *lit_plan = malloc(sizeof(CallArgPlan));
    Type *type = arg->type;

    lit_plan->arg = arg_plan->arg;
    lit_plan->kind = AGGREGATE;
    lit_plan->align = align_of(type);
    lit_plan->size = size_of(type);

    *cur = align_up(*cur, lit_plan->align);
    lit_plan->offset = *cur;
    *cur += lit_plan->size;
    arg_plan->storage_plan = lit_plan;
}

static void plan_params_args(NodeList args, Type *ret_type, CallPlan *plan) {
    if (!plan) return;
    
    size_t current = (plan->is_builtin_print || is_aggregate(ret_type)) ? 8 : 0;
    
    for (size_t i = 0; i < args.length; ++i) {
        CallArgPlan arg_plan = (CallArgPlan){0};
        Type *type = args.data[i]->type;

        arg_plan.arg = args.data[i];
        arg_plan.kind = is_scalar(type) ? SCALAR : AGGREGATE;
        arg_plan.align = pa_align_of(type);
        arg_plan.size = pa_size_of(type);

        current = align_up(current, arg_plan.align);
        arg_plan.offset = current;
        current += arg_plan.size;
        plan->args[i] = arg_plan;
    }

    for (size_t i = 0; i < args.length; ++i) {
        if (plan->args[i].arg->kind == NK_STRING_LIT || plan->args[i].arg->kind == NK_ARRAY_LIT) {
            eval_storage_for_temp_args(&plan->args[i], &current);
        }
    }

    plan->arg_count = args.length;
    plan->outgoing_stack_size = align_up(current, 16);
}

static void gen_call_args_to_stack(CodegenCtx *ctx, CallPlan *plan) {
    if (!plan) return;

    emit(ctx, "\tsub rsp, %zu\n", plan->outgoing_stack_size);

    if (plan->is_builtin_print) {
        emit(ctx, "\tmov qword [rsp], %zu\n", plan->arg_count);
    }

    for (size_t i = 0; i < plan->arg_count; ++i) {
        CallArgPlan ap = plan->args[i];
        
        if (ap.kind == SCALAR) {
            gen_expr(ctx, ap.arg);
            if (ap.size == 8) {
                emit(ctx, "\tmov [rsp + %zu], rax\n", ap.offset);
            } else {
                emit(ctx, "\tmov [rsp + %zu], eax\n", ap.offset);
            }
        } else {
            emit(ctx, "\tmov r11, rsp\n");
            emit(ctx, "\tadd r11, %zu\n", ap.offset);
            if (ap.arg->kind == NK_ARRAY_LIT || ap.arg->kind == NK_STRING_LIT) {
                emit(ctx, "\tmov r9, rsp\n");
                emit(ctx, "\tadd r9, %zu\n", ap.storage_plan->offset);
                emit(ctx, "\tmov [r11], r9\n");
                gen_aggregate_expr_into(ctx, ap.arg);
            } else if (ap.arg->type->kind == T_STRUCT) {
                gen_addr(ctx, ap.arg);
                emit_copy_bytes(ctx, "r11", "rax", ap.size);
            } else {
                gen_addr(ctx, ap.arg);
                emit(ctx, "\tmov [r11], rax\n");
            }
        }
    }   
}

static void gen_store_incoming_params(CodegenCtx *ctx, Node *fn) {
    if (!fn) return;

    CallPlan plan = (CallPlan){0};
    plan_params_args(fn->fn.params, fn->type->fn.ret, &plan);
    
    if (is_aggregate(fn->type->fn.ret)) {
        emit(ctx, "\tlea r8, [rbp + 16]\n");
        emit(ctx, "\tlea r9, [rbp - 8]\n");
        emit(ctx, "\tmov r10, qword [r8]\n");
        emit(ctx, "\tmov qword [r9], r10\n");
    }

    for (size_t i = 0; i < plan.arg_count; ++i) {
        CallArgPlan ap = plan.args[i];
        Symbol *sym = fn->fn.params.data[i]->param.sym;
        emit(ctx, "\tlea r8, [rbp + 16 + %zu]\n", ap.offset);
        emit(ctx, "\tlea r9, [rbp - %zu]\n", sym->stack_offset);

        if (ap.kind == SCALAR) {
            if (ap.size == 8) {
                emit(ctx, "\tmov r10, qword [r8]\n");
                emit(ctx, "\tmov qword [r9], r10\n");
            } else {
                emit(ctx, "\tmov r10d, dword [r8]\n");
                emit(ctx, "\tmov dword [r9], r10d\n");
            }
        } else {
            emit_copy_bytes(ctx, "r9", "r8", ap.size);
        }
    }
}

static void gen_binary_arith(CodegenCtx *ctx, Node *expr) {
    if (!expr) return;
    
    gen_expr(ctx, expr->binary.rhs);
    emit(ctx, "\tpush rax\n");
    gen_expr(ctx, expr->binary.lhs);
    emit(ctx, "\tpop rcx\n");
    
    size_t sz = size_of(expr->type);

    switch (expr->binary.op.type) {
        case TOK_PLUS:{
            if (sz == 4) emit(ctx, "\tadd eax, ecx\n");
            else emit(ctx, "\tadd rax, rcx\n");
            return;
        }
        case TOK_MINUS:{
            if (sz == 4) emit(ctx, "\tsub eax, ecx\n");
            else emit(ctx, "\tsub rax, rcx\n");
            return;
        }
        case TOK_STAR:{
            if (sz == 4) emit(ctx, "\timul eax, ecx\n");
            else emit(ctx, "\timul rax, rcx\n");
            return;
        }
        case TOK_SLASH:
        case TOK_PERCENT:{
            bool is_signed = is_signed_int(expr->type);
            
            if (sz == 4) {
                if (is_signed) {
                    emit(ctx, "\tcdq\n");
                    emit(ctx, "\tidiv ecx\n");
                } else {
                    emit(ctx, "\txor edx, edx\n");
                    emit(ctx, "\tdiv ecx\n");
                }
                if (expr->binary.op.type == TOK_PERCENT) emit(ctx, "\tmov eax, edx\n");
            } else {
                if (is_signed) {
                    emit(ctx, "\tcqo\n");
                    emit(ctx, "\tidiv rcx\n");
                } else {
                    emit(ctx, "\txor rdx, rdx\n");
                    emit(ctx, "\tdiv rcx\n");
                }
                if (expr->binary.op.type == TOK_PERCENT) emit(ctx, "\tmov rax, rdx\n");
            }
            return;
        }
        default: return;
    }
}

static void gen_binary_compare(CodegenCtx *ctx, Node *expr) {
    if (!expr) return;

    gen_expr(ctx, expr->binary.rhs);
    emit(ctx, "\tpush rax\n");
    gen_expr(ctx, expr->binary.lhs);
    emit(ctx, "\tpop rcx\n");
    
    size_t sz = size_of(expr->binary.lhs->type);
    if (sz == 8) emit(ctx, "\tcmp rax, rcx\n");
    else emit(ctx, "\tcmp eax, ecx\n");

    switch (expr->binary.op.type) {
        case TOK_EQ_EQ:{
            emit(ctx, "\tsete al\n");
            break;
        }
        case TOK_NOT_EQ:{
            emit(ctx, "\tsetne al\n");
            break;
        }
        case TOK_GT:{
            if (is_signed_int(expr->binary.lhs->type)) emit(ctx, "\tsetg al\n");
            else emit(ctx, "\tseta al\n");
            break;
        }
        case TOK_LT:{
            if (is_signed_int(expr->binary.lhs->type)) emit(ctx, "\tsetl al\n");
            else emit(ctx, "\tsetb al\n");
            break;
        }
        case TOK_GT_EQ:{
            if (is_signed_int(expr->binary.lhs->type)) emit(ctx, "\tsetge al\n");
            else emit(ctx, "\tsetae al\n");
            break;
        }
        case TOK_LT_EQ:{
            if (is_signed_int(expr->binary.lhs->type)) emit(ctx, "\tsetle al\n");
            else emit(ctx, "\tsetbe al\n");
            break;
        }
        default: return;
    }
    emit(ctx, "\tmovzx eax, al\n");
}

static void gen_binary_logical(CodegenCtx *ctx, Node *expr) {
    if (!expr) return;
    
    int label_num = ctx->next_label++;

    gen_expr(ctx, expr->binary.lhs);
    emit(ctx, "\tcmp eax, 0\n");

    if (expr->binary.op.type == TOK_AMP_AMP) {
        emit(ctx, "\tje .Lfalse%d\n", label_num);
        gen_expr(ctx, expr->binary.rhs);
        emit(ctx, "\tjmp .Lend%d\n", label_num);
        emit(ctx, ".Lfalse%d:\n", label_num);
        emit(ctx, "\tmov eax, 0\n");
    } else {
        emit(ctx, "\tjne .Ltrue%d\n", label_num);
        gen_expr(ctx, expr->binary.rhs);
        emit(ctx, "\tjmp .Lend%d\n", label_num);
        emit(ctx, ".Ltrue%d:\n", label_num);
        emit(ctx, "\tmov eax, 1\n");
    }
    emit(ctx, ".Lend%d:\n", label_num);
}

static bool is_logical_binop(TokenType op) {
    return op == TOK_AMP_AMP || op == TOK_PIPE_PIPE;
}

static bool is_compare_binop(TokenType op) {
    switch (op) {
        case TOK_EQ_EQ:
        case TOK_NOT_EQ:
        case TOK_GT:
        case TOK_LT:
        case TOK_GT_EQ:
        case TOK_LT_EQ:
            return true;
        default:
            return false;
    }
}

static bool is_arith_binop(TokenType op) {
    switch (op) {
        case TOK_PLUS:
        case TOK_MINUS:
        case TOK_STAR:
        case TOK_SLASH:
        case TOK_PERCENT:
            return true;
        default:
            return false;
    }
}

static void gen_expr(CodegenCtx *ctx, Node *expr) {
    if (!expr) return;

    switch (expr->kind) {
        case NK_CAST:{
            Node *inner = expr->cast.expr;
            Type *src = inner->type;
            Type *dst = expr->type;
            gen_expr(ctx, inner);

            size_t ssz = size_of(src);
            size_t dsz = size_of(dst);

            if (ssz == dsz) {
                ;
            } else if (ssz == 4 && dsz == 8) {
                if (is_signed_int(src)) {
                    emit(ctx, "\tmovsxd rax, eax\n");
                } else {
                    ;
                }
            } else if (ssz == 8 && dsz == 4) {
                ;
            }
            break;
        }
        case NK_UNARY: {
            gen_expr(ctx, expr->unary.expr);
            size_t sz = size_of(expr->type);

            if (expr->unary.op.type == TOK_PLUS) {
                ;
            } else if (expr->unary.op.type == TOK_MINUS) {
                if (sz == 4) emit(ctx, "\tneg eax\n");
                else emit(ctx, "\tneg rax\n");
            } else if (expr->unary.op.type == TOK_NOT) {
                emit(ctx, "\txor eax, 1\n");
            }
            break;
        }
        case NK_BINARY:{
            TokenType op = expr->binary.op.type;
            
            if (is_logical_binop(op)) {
                gen_binary_logical(ctx, expr);
            } else if (is_compare_binop(op)) {
                gen_binary_compare(ctx, expr);
            } else if (is_arith_binop(op)) {
                gen_binary_arith(ctx, expr);
            }
            break;
        }
        case NK_NUMBER:{
            size_t sz = size_of(expr->type);

            if (sz == 4) {
                emit(ctx, "\tmov eax, %llu\n", expr->number.value);
            } else {
                emit(ctx, "\tmov rax, %llu\n", expr->number.value);
            }
            break;
        }
        case NK_IDENT:{
            gen_addr(ctx, expr);
            size_t sz = size_of(expr->type);
            
            if (sz == 4) {
                emit(ctx, "\tmov eax, dword [rax]\n");
            } else {
                emit(ctx, "\tmov rax, qword [rax]\n");
            }
            break;
        }
        case NK_FIELD:{
            gen_addr(ctx, expr);
            Node *field = expr->field.field_decl->field_decl;

            if (field->stc_field.size == 4) {
                emit(ctx, "\tmov eax, dword [rax]\n");
            } else {
                emit(ctx, "\tmov rax, qword [rax]\n");
            }
            break;
        }
        case NK_INDEX:{
            gen_addr(ctx, expr);
            Type *t = expr->index_.array->type;
            size_t elem_size = (t->kind == T_STRING) ? 1 : size_of(expr->type);
            
            if (elem_size == 1) {
                emit(ctx, "\tmovzx eax, byte [rax]\n");
            } else if (elem_size == 4) {
                emit(ctx, "\tmov eax, dword [rax]\n");
            } else {
                emit(ctx, "\tmov rax, qword [rax]\n");
            }
            break;
        }
        case NK_CALL:{
            Symbol *sym = expr->call.resolved_fn;
            CallPlan plan = (CallPlan){0};

            if (sym->length == 5 && !strncmp(sym->start_pos, "print", 5)) {
                plan.is_builtin_print = true;
            }
            
            plan_params_args(expr->call.args, sym->type->fn.ret, &plan);
            gen_call_args_to_stack(ctx, &plan);

            emit(ctx, "\tcall %.*s\n", (int)sym->length, sym->start_pos);
            emit(ctx, "\tadd rsp, %zu\n", plan.outgoing_stack_size);
            break;
        }
        case NK_BOOLEAN:{
            emit(ctx, "\tmov eax, %zu\n", expr->boolean.value);
        }
        default: break;
    }
}

static void gen_aggregate_expr_into(CodegenCtx *ctx, Node *expr) {
    if (!expr) return;

    size_t sz = size_of(expr->type);
    switch (expr->kind) {
        case NK_CALL:{
            Symbol *sym = expr->call.resolved_fn;
            CallPlan plan = (CallPlan){0};
            
            plan_params_args(expr->call.args, sym->type->fn.ret, &plan);
            gen_call_args_to_stack(ctx, &plan);

            emit(ctx, "\tmov qword [rsp], r9\n");
            emit(ctx, "\tcall %.*s\n", (int)sym->length, sym->start_pos);
            emit(ctx, "\tadd rsp, %zu\n", plan.outgoing_stack_size);
            break;
        }
        case NK_IDENT:
        case NK_INDEX:
        case NK_FIELD:{
            gen_addr(ctx, expr);
            emit_copy_bytes(ctx, "r9", "rax", sz);
            break;
        }
        case NK_ARRAY_LIT:{
            NodeList list = expr->array_lit.elems;
            Type *elem_ty = expr->type->array.elem;
            size_t elem_sz = size_of(elem_ty);

            for (size_t i = 0; i < list.length; ++i) {
                Node *n = list.data[i];
                if (is_scalar(n->type)) {
                    gen_expr(ctx, n);
                    if (elem_sz == 8) {
                        emit(ctx, "\tmov [r9], rax\n");
                    } else {
                        emit(ctx, "\tmov [r9], eax\n");
                    }
                } else {
                    gen_aggregate_expr_into(ctx, n);
                }
                emit(ctx, "\tadd r9, %zu\n", elem_sz);
            }
            break;
        }
        case NK_STRING_LIT:{
            Token str = expr->string_lit.value;
            for (size_t i = 0; i < str.lit_length_bytes; ++i) {
                unsigned char c = (unsigned char)str.lit[i];
                emit(ctx, "\tmov byte [r9], %u\n", c);
                emit(ctx, "\tinc r9\n");
            }
            break;
        }
        default: break;
    }
}

static void gen_stmt(CodegenCtx *ctx, Node *stmt) {
    if (!stmt) return;

    switch (stmt->kind) {
        case NK_BLOCK:{
            for (size_t i = 0; i < stmt->stmts.length; ++i) {
                gen_stmt(ctx, stmt->stmts.data[i]);
            }
            break;
        }
        case NK_BREAK:{
            emit(ctx, "\tjmp %s\n", ctx->labels.label[ctx->labels.top]);
            break;
        }
        case NK_WHILE:{
            int label_num = ctx->next_label++;
            strcpy(ctx->labels.label[++ctx->labels.top], ".Lend");
            sprintf(ctx->labels.label[ctx->labels.top] + 5, "%d", label_num);

            emit(ctx, ".Lstart%d:\n", label_num);
            gen_expr(ctx, stmt->while_loop.cond);
            emit(ctx, "\tcmp eax, 0\n");
            emit(ctx, "\tje .Lend%d\n", label_num);
            
            gen_stmt(ctx, stmt->while_loop.stmt);
            emit(ctx, "\tjmp .Lstart%d\n", label_num);
            emit(ctx, ".Lend%d:\n", label_num);
            --ctx->labels.top;
            break;
        }
        case NK_FOR:{
            int label_num = ctx->next_label++;
            strcpy(ctx->labels.label[++ctx->labels.top], ".Lend");
            sprintf(ctx->labels.label[ctx->labels.top] + 5, "%d", label_num);

            for (size_t i = 0; i < stmt->for_stmt.init.length; ++i){
                gen_stmt(ctx, stmt->for_stmt.init.data[i]);
            }
            emit(ctx, ".Lstart%d:\n", label_num);
            
            if (stmt->for_stmt.expr) {
                gen_expr(ctx, stmt->for_stmt.expr);
                emit(ctx, "\tcmp eax, 0\n");
                emit(ctx, "\tje .Lend%d\n", label_num);
            }
            gen_stmt(ctx, stmt->for_stmt.stmt);
            for (size_t i = 0; i < stmt->for_stmt.step.length; ++i){
                gen_stmt(ctx, stmt->for_stmt.step.data[i]);
            }
            emit(ctx, "\tjmp .Lstart%d\n", label_num);
            emit(ctx, ".Lend%d:\n", label_num);
            --ctx->labels.top;
            break;
        }
        case NK_IF:{
            int label_num = ctx->next_label++;
            gen_expr(ctx, stmt->if_stmt.cond);
            emit(ctx, "\tcmp eax, 0\n");
            
            if (!stmt->if_stmt.else_chain) emit(ctx, "\tje .Lend%d\n", label_num);
            else emit(ctx, "\tje .Lelse%d\n", label_num);

            gen_stmt(ctx, stmt->if_stmt.stmt);
            
            if (stmt->if_stmt.else_chain) {
                emit(ctx, "\tjmp .Lend%d\n", label_num);
                emit(ctx, ".Lelse%d:\n", label_num);
                gen_stmt(ctx, stmt->if_stmt.else_chain);
            }

            emit(ctx, ".Lend%d:\n", label_num);
            break;
        }
        case NK_RETURN:{
            if (!stmt->return_stmt || is_scalar(stmt->return_stmt->type)) {
                gen_expr(ctx, stmt->return_stmt);
            } else {
                gen_addr(ctx, stmt->return_stmt);
                size_t sz = size_of(stmt->return_stmt->type);
                emit(ctx, "\tmov r11, [rbp - 8]\n");
                emit_copy_bytes(ctx, "r11", "rax", sz);
            }
            emit(ctx, "\tjmp .epilogue\n");
            break;
        }
        case NK_ASSIGN:{
            Node *lvalue = stmt->assign.lvalue;
            Node *rvalue = stmt->assign.rvalue;

            size_t sz = (lvalue->kind == NK_INDEX && lvalue->index_.array->type->kind == T_STRING) ? 1 : size_of(lvalue->type);

            gen_addr(ctx, lvalue);
            emit(ctx, "\tpush rax\n");

            if (is_scalar(lvalue->type)) {
                gen_expr(ctx, rvalue);
                emit(ctx, "\tpop r9\n");
                if (sz == 1) {
                    emit(ctx, "\tmov byte [r9], al\n");
                } else if (sz == 4) {
                    emit(ctx, "\tmov dword [r9], eax\n");
                } else {
                    emit(ctx, "\tmov qword [r9], rax\n");
                }
            } else {
                emit(ctx, "\tpop r9\n");
                gen_aggregate_expr_into(ctx, rvalue);
            }
            break;
        }
        case NK_DECLEXPR: {
            Symbol *sym = stmt->declexpr.sym;
            Node *init  = stmt->declexpr.rvalue;
            
            if (!init) break;
            
            size_t sz = size_of(sym->type);
            if (is_scalar(sym->type)) {
                gen_expr(ctx, init);
                if (sz == 4) {
                    emit(ctx, "\tmov dword [rbp - %zu], eax\n", sym->stack_offset);
                } else {
                    emit(ctx, "\tmov qword [rbp - %zu], rax\n", sym->stack_offset);
                }
            } else {
                emit(ctx, "\tlea r9, [rbp - %zu]\n", sym->stack_offset);
                gen_aggregate_expr_into(ctx, init);
            }
            break;
        }
        case NK_EXPR:{
            gen_expr(ctx, stmt->expr_stmt);
            break;
        }
        default: break;
    }
}

static void layout_stmt(CodegenCtx *ctx, Node *stmt, size_t *current) {
    if (!stmt || !current) return;

    switch (stmt->kind) {
        case NK_BLOCK:{
            for (size_t i = 0; i < stmt->stmts.length; ++i) {
                layout_stmt(ctx, stmt->stmts.data[i], current);
            }
            break;
        }
        case NK_WHILE:{
            layout_stmt(ctx, stmt->while_loop.stmt, current);
            break;
        }
        case NK_FOR:{
            for (size_t i = 0; i < stmt->for_stmt.init.length; ++i) {
                layout_stmt(ctx, stmt->for_stmt.init.data[i], current);
            }
            layout_stmt(ctx, stmt->for_stmt.stmt, current);
            break;
        }
        case NK_IF:{
            layout_stmt(ctx, stmt->if_stmt.stmt, current);
            if (stmt->if_stmt.else_chain) layout_stmt(ctx, stmt->if_stmt.else_chain, current);
            break;
        }
        case NK_DECLEXPR:{
            Symbol *sym = stmt->declexpr.sym;
            *current = align_up(*current, align_of(sym->type));
            *current += size_of(sym->type);
            sym->stack_offset = *current;
            break;
        }
        default: return;
    }
}

static void layout_params(NodeList params, size_t *current) {
    for (size_t i = 0; i < params.length; ++i) {
        Symbol *sym = params.data[i]->param.sym;
        *current = align_up(*current, pa_align_of(sym->type));
        *current += pa_size_of(sym->type);
        sym->stack_offset = *current;
    }
}

static size_t layout_function(CodegenCtx *ctx, Node *fn) {
    if (!fn) return 0;

    size_t current = (is_scalar(fn->type->fn.ret)) ? 0 : 8;
    layout_params(fn->fn.params, &current);
    layout_stmt(ctx, fn->fn.stmt, &current);

    return align_up(current, 16);
}

static void gen_function(CodegenCtx *ctx, Node *fn) {
    if (!fn->fn.stmt) return;
    emit(ctx, "global %.*s\n", (int)fn->fn.name.length, fn->fn.name.start_pos);
    emit(ctx, "%.*s:\n", (int)fn->fn.name.length, fn->fn.name.start_pos);
    emit(ctx, "\tpush rbp\n");
    emit(ctx, "\tmov rbp, rsp\n");

    size_t frame_size = layout_function(ctx, fn);
    if (frame_size != 0) emit(ctx, "\tsub rsp, %zu\n", frame_size);
    gen_store_incoming_params(ctx, fn);

    gen_stmt(ctx, fn->fn.stmt);

    emit(ctx, ".epilogue:\n");
    emit(ctx, "\tmov rsp, rbp\n");
    emit(ctx, "\tpop rbp\n");
    emit(ctx, "\tret\n\n");
}

static void struct_layout(Node *strc) {
    NodeList fields = strc->struct_.fields;
    size_t current = 0;
    size_t max_align = 0;

    for (size_t i = 0; i < fields.length; ++i) {
        Node *field = fields.data[i];
        current = align_up(current, align_of(field->type));
        if (align_of(field->type) > max_align) max_align = align_of(field->type);
        field->stc_field.field_offset = current;
        field->stc_field.size = size_of(field->type);
        current += size_of(field->type);
    }
    strc->struct_.align = max_align;
    strc->struct_.size = align_up(current, max_align);
}

static void gen_print_fn(CodegenCtx *ctx) {
    emit(ctx, "global print\n");
    emit(ctx, "print:\n");
    emit(ctx, "\tpush rbp\n");
    emit(ctx, "\tmov rbp, rsp\n");

    emit(ctx, "\tmov r8, qword [rbp + 16]\n");
    emit(ctx, "\tmov r9, 24\n");

    emit(ctx, ".args_loop:\n");
    emit(ctx, "\tcmp r8, 0\n");
    emit(ctx, "\tje .epilogue_print\n");

    emit(ctx, "\tmov r10, 0\n");
    emit(ctx, "\tmov r11, [rbp + r9]\n");
    
    emit(ctx, ".start_str_loop:\n");
    emit(ctx, "\tcmp byte [r11 + r10], 0\n");
    emit(ctx, "\tje .end_str_loop\n");
    emit(ctx, "\tinc r10\n");
    emit(ctx, "\tjmp .start_str_loop\n");

    emit(ctx, ".end_str_loop:\n");
    emit(ctx, "\tmov rax, 1\n");
    emit(ctx, "\tmov rdi, 1\n");
    emit(ctx, "\tmov rsi, r11\n");
    emit(ctx, "\tmov rdx, r10\n");
    emit(ctx, "\tsyscall\n");

    emit(ctx, "\tdec r8\n");
    emit(ctx, "\tadd r9, 8\n");
    emit(ctx, "\tjmp .args_loop\n");

    emit(ctx, ".epilogue_print:\n");
    emit(ctx, "\tmov rsp, rbp\n");
    emit(ctx, "\tpop rbp\n");
    emit(ctx, "\tret\n\n");
}

static void gen_start(CodegenCtx *ctx) {
    emit(ctx, "global _start\n");
    emit(ctx, "_start:\n");
    emit(ctx, "\tcall main\n");
    emit(ctx, "\tmov rdi, rax\n");
    emit(ctx, "\tmov rax, 60\n");
    emit(ctx, "\tsyscall\n\n");
}

void codegen_program(Node *n, FILE *out) {
    CodegenCtx ctx = (CodegenCtx){0};
    ctx.out = out;
    
    gen_start(&ctx);
    gen_print_fn(&ctx);
    
    for (size_t i = 0; i < n->program.length; ++i) {
        if (n->program.data[i]->kind == NK_FN) gen_function(&ctx, n->program.data[i]);
        if (n->program.data[i]->kind == NK_STRUCT) struct_layout(n->program.data[i]);
    }
}