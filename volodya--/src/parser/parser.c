#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include "../Token/TokenNames.h"
#include "parser.h"

typedef enum {
    SYNC_STMT,
    SYNC_EXPR,
    SYNC_PAREN,
    SYNC_BRACE,
    SYNC_BLOCK,
    SYNC_BRACKET,
    SYNC_TOPLEVEL
} SyncKind;

typedef struct {
    size_t pos;
    const char *file;
    size_t err_count;
    TokenVec *vec;
    int recovering;
    size_t last_error_pos;
} ParserContext;

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

static inline void begin_recovery(ParserContext *p) { p->recovering = 1; p->last_error_pos = p->pos; }
static inline void end_recovery(ParserContext *p) { p->recovering = 0; }
static inline int should_report(ParserContext *p, Token at) {
    return !p->recovering || p->pos != p->last_error_pos || at.type == TOK_EOF;
}

static void error_tok(ParserContext *p, Token t, const char *msg) {
    if (!should_report(p, t)) return;
    if (p->err_count > 10) { fprintf(stderr, "too many errors\n"); exit(1); }
    fprintf(stderr, "%s:%zu:%zu: syntax error: %s\n", p->file, t.line, t.column, msg);
    p->err_count++;
    begin_recovery(p);
}

static int is_stmt_sync(TokenType t) { return t==TOK_SEMICOLON || t==TOK_RBRACE || t==TOK_EOF; }
static int is_expr_sync(TokenType t) { 
    return t==TOK_COMMA || t==TOK_RPAREN || t==TOK_RBRACKET || 
    t==TOK_SEMICOLON || t==TOK_RBRACE || t==TOK_EOF;
}
static int is_paren_sync(TokenType t) { return t==TOK_RPAREN || t==TOK_SEMICOLON || t==TOK_RBRACE || t==TOK_EOF; }
static int is_brace_sync(TokenType t) { return t==TOK_RBRACE || t==TOK_EOF; }
static int is_block_sync(TokenType t) { return t==TOK_LBRACE || t==TOK_EOF; }
static int is_brack_sync(TokenType t) { return t==TOK_RBRACKET || t==TOK_EOF; }
static int is_top_sync(TokenType t) { return t==TOK_KW_FN || t==TOK_KW_STRUCT || t==TOK_EOF; }

static void sync(ParserContext *p, SyncKind k) {
    int (*pred)(TokenType) = NULL;
    switch (k) {
        case SYNC_STMT: pred = is_stmt_sync; break;
        case SYNC_EXPR: pred = is_expr_sync; break;
        case SYNC_PAREN: pred = is_paren_sync; break;
        case SYNC_BRACE: pred = is_brace_sync; break;
        case SYNC_BLOCK: pred = is_block_sync;break;
        case SYNC_BRACKET: pred = is_brack_sync; break;
        case SYNC_TOPLEVEL: pred = is_top_sync; break;
    }
    while (!pred(peek(p).type)) { if (peek(p).type == TOK_EOF) break; p->pos++; }
    if (k == SYNC_STMT && peek(p).type == TOK_SEMICOLON) consume(p);
    end_recovery(p);
}

static int expect_sync(ParserContext *p, TokenType expected, SyncKind k) {
    if (match(p, expected)) return 1;
    char buf[96];
    snprintf(buf, sizeof(buf), "expected %s, saw %s", token_type_name(expected), token_type_name(peek(p).type));
    error_tok(p, peek(p), buf);
    sync(p, k);
    return 0;
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

static Node* mk_error(const char *error) {
    Node *n = mk(NK_ERROR);
    n->error = error;
    return n;
}

static Node *mk_var(Token t) {
    Node *n = mk(NK_VAR);
    n->var = t;
    return n;
}

static Node *mk_string(Token t) {
    Node *n = mk(NK_STRING);
    n->string = t;
    return n;
}

static Node *mk_number(long long num, Token t) {
    Node *n = mk(NK_NUMBER);
    n->number.value = num;
    n->number.t = t;
    return n;
}

static Node *mk_boolean(size_t value, Token t) {
    Node *n = mk(NK_BOOLEAN);
    n->boolean.value = value;
    n->boolean.t = t;
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

static Node *mk_cast(Node *cast_type, Node *expr) {
    Node *n = mk(NK_CAST);
    n->cast.cast_type = cast_type;
    n->cast.expr = expr;
    return n;
}

static Node *mk_assign(Node *lvalue, Node *rvalue) {
    Node *n = mk(NK_ASSIGN);
    n->assign.lvalue = lvalue; 
    n->assign.rvalue = rvalue;
    return n;
}

static Node *mk_declexpr(bool is_const, Node *type, Token name, Node *rvalue) {
    Node *n = mk(NK_DECLEXPR);
    n->declexpr.is_const = is_const;
    n->declexpr.type = type;
    n->declexpr.name = name;
    n->declexpr.rvalue = rvalue;
    return n;
}

static Node *mk_while(Node *cond, Node *stmt) {
    Node *n = mk(NK_WHILE);
    n->while_loop.cond = cond;
    n->while_loop.stmt = stmt;
    return n;
}

static Node *mk_return(Node *expr) {
    Node *n = mk(NK_RETURN);
    n->return_stmt = expr;
    return n;
}

static Node *mk_if(Node *cond, Node *stmt) {
    Node *n = mk(NK_IF);
    n->if_stmt.cond = cond;
    n->if_stmt.stmt = stmt;
    n->if_stmt.else_chain = NULL;
    return n;
}

static Node *mk_for(NodeList forinit, Node *cond, NodeList forstep, Node *body) {
    Node *n = mk(NK_FOR);
    n->for_stmt.init = forinit;
    n->for_stmt.expr = cond;
    n->for_stmt.step = forstep;
    n->for_stmt.stmt = body;
    return n;        
}

static Node *mk_block(NodeList stmt_list) {
    Node *n = mk(NK_BLOCK);
    n->stmts = stmt_list;
    return n;
}

static Node *mk_expr(Node *e) {
    Node *n = mk(NK_EXPR);
    n->expr_stmt = e;
    return n;
}

static Node *mk_param(Token name, Node *type) {
    Node *n = mk(NK_PARAM);
    n->param.name = name;
    n->param.p_type = type;
    return n;
}

static Node *mk_fn(Node *ret_type, Token name, NodeList p_list, Node *stmt) {
    Node *n = mk(NK_FN);
    n->fn.ret_type = ret_type;
    n->fn.name = name;
    n->fn.params = p_list;
    n->fn.stmt = stmt;
    return n;
}

static Node *mk_stc_field(Token name, Node *type) {
    Node *n = mk(NK_STC_FIELD);
    n->stc_field.name = name;
    n->stc_field.type = type;
    return n;
}

static Node *mk_struct(Token name, NodeList f_list) {
    Node *n = mk(NK_STRUCT);
    n->struct_.name = name;
    n->struct_.fields = f_list;
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

static Node *parse_expr(ParserContext *p, size_t min_bp);

static inline int is_closer_or_sep(TokenType t){
    return t==TOK_RPAREN || t==TOK_RBRACKET || t==TOK_RBRACE || t==TOK_COMMA || t==TOK_SEMICOLON;
}

static Node *err_eof(const char *a, ParserContext *p) {
    error_tok(p, peek(p), a);
    return mk_error(a);
}

static Node *convert(Token t, unsigned base) {
    const char *s = t.start_pos;
    size_t n = t.length;

    if (base < 2 || base > 16) return mk_error("out of base err");

    unsigned long long acc = 0;

    for (size_t i = 0; i < n; ++i) {
        unsigned char ch = (unsigned char)s[i];
        if (ch == '_') continue;

        unsigned digit;
        if (ch >= '0' && ch <= '9') digit = ch - '0';
        else if (ch >= 'a' && ch <= 'f') digit = ch - 'a' + 10;
        else if (ch >= 'A' && ch <= 'F') digit = ch - 'A' + 10;

        __uint128_t wide = (__uint128_t)acc * base + digit;
        if (wide > UINT64_MAX) return mk_error("overflow err");
        acc = (unsigned long long)wide;
    }

    return mk_number(acc, t);
}

static Node *parse_primary_postfix(ParserContext *p) {
    Token t = peek(p);
    Node *n = NULL;

    switch (t.type) {
        case TOK_IDENT: n = mk_var(consume(p)); break;
        case TOK_DEC_LIT: n = convert(consume(p), 10); break;
        case TOK_HEX_LIT: n = convert(consume(p), 16); break;
        case TOK_BIN_LIT: n = convert(consume(p), 2); break;
        case TOK_OCT_LIT: n = convert(consume(p), 8); break;
        case TOK_STRING_LIT: n = mk_string(consume(p)); break;
        case TOK_KW_TRUE: n = mk_boolean(1, consume(p)); break;
        case TOK_KW_FALSE: n = mk_boolean(0, consume(p)); break;
        case TOK_LPAREN:{
            consume(p);
            size_t err0 = p->err_count;
            n = parse_expr(p, 0);
            if (p->err_count > err0) sync(p, SYNC_PAREN);
            expect_sync(p, TOK_RPAREN, SYNC_PAREN);
            break;
        }
        case TOK_LBRACKET:{
            consume(p);
            NodeList args = {NULL, 0, 0};
            n = mk(NK_ARRAY_LIT);
            Token tk = peek(p);
            if (tk.type != TOK_RBRACKET) {
                size_t err_before = p->err_count;
                push(&args, parse_expr(p, 0));
                
                if (err_before < p->err_count) {
                    sync(p, SYNC_BRACKET);
                    return mk_error("array lit err");
                }

                while (match(p, TOK_COMMA)) {
                    Token tokn = peek(p);
                    if (tokn.type == TOK_RBRACKET) {
                        error_tok(p, tokn, "trailing comma in element list is not allowed");
                        break;
                    } else if (tokn.type == TOK_EOF) {
                        return err_eof("unexpected end of input, expected expression or ']'", p);
                    } else if (tokn.type == TOK_COMMA) {
                        error_tok(p, tokn, "multiple commas in element list are not allowed");
                        sync(p, SYNC_BRACKET);
                        break;
                    }
                    push(&args, parse_expr(p, 0));
                }
            }
            
            expect_sync(p, TOK_RBRACKET, SYNC_BRACKET);
            n->array_lit = args; break;
        }
        default:
            error_tok(p, peek(p), "expected expression");
            if (!is_closer_or_sep(t.type)) consume(p);
            return mk_error("expression err");
    }

    bool flag = false;
    for (;;) {
        Token t2 = peek(p);
        if (t2.type == TOK_LPAREN) {
            flag = false;
            consume(p);
            if (peek(p).type == TOK_EOF) {
                return err_eof("unexpected end of input, expected expression or ')'", p);
            }
            NodeList args = {NULL, 0, 0};
            if (peek(p).type != TOK_RPAREN) {
                size_t err_before = p->err_count;
                push(&args, parse_expr(p, 0));
                
                if (err_before < p->err_count) {
                    sync(p, SYNC_PAREN);
                }

                while (match(p, TOK_COMMA)) {
                    Token tok = peek(p);
                    if (tok.type == TOK_RPAREN) {
                        error_tok(p, tok, "trailing comma in argument list is not allowed");
                        break;
                    } else if (tok.type == TOK_EOF) {
                        return err_eof("unexpected end of input, expected expression or ')'", p);
                    } else if (tok.type == TOK_COMMA) {
                        error_tok(p, tok, "multiple commas in argument list are not allowed");
                        sync(p, SYNC_PAREN);
                        break;
                    }
                    push(&args, parse_expr(p, 0));
                }
            }
            expect_sync(p, TOK_RPAREN, SYNC_PAREN);
            n = mk_call(n);
            n->call.args = args;
        } else if (t2.type == TOK_LBRACKET) {
            consume(p);
            size_t err_before = p->err_count;
            n = mk_index(n, parse_expr(p, 0));
            if (peek(p).type == TOK_EOF) {
                return err_eof("unexpected end of input, expected expression or ']'", p);
            }
            if (err_before < p->err_count) {
                sync(p, SYNC_BRACKET);
            }
            expect_sync(p, TOK_RBRACKET, SYNC_BRACKET);
            if (peek(p).type == TOK_LBRACKET) {
                error_tok(p, peek(p), "multi-demensional arrays are not supported");
                sync(p, SYNC_PAREN); break;
            }
        } else if (t2.type == TOK_DOT && peek_next(p).type == TOK_IDENT) {
            flag = false;
            consume(p);
            n = mk_field(n, consume(p));
        } else break;
    }

    return n;
}

static inline int is_unary(Token t) {
    return t.type == TOK_MINUS || t.type == TOK_NOT || t.type == TOK_PLUS;
}

static inline int is_bitype(Token t) {
    return t.type == TOK_KW_I32 || t.type == TOK_KW_I64 || t.type == TOK_KW_U32 ||
        t.type == TOK_KW_U64 || t.type == TOK_KW_BOOL || t.type == TOK_KW_STRING;
}

static int try_parse_cast(ParserContext *p, Node *cast_type) {
    size_t tmp = p->pos;
    if (consume(p).type != TOK_LPAREN) { p->pos = tmp; return 0; }
    Token t = peek(p);

    if (is_bitype(t) || t.type == TOK_IDENT) cast_type = mk_var(consume(p));
    else { p->pos = tmp; return 0; }
    
    if (peek(p).type == TOK_LBRACKET && peek_next(p).type == TOK_RBRACKET) {
        consume(p);
        cast_type = (cast_type, mk_number(-1, consume(p)));
        if (peek(p).type == TOK_LBRACKET) { p->pos = tmp; return 0; }
    }

    if (match(p, TOK_RPAREN)) { return 1; }
    else { p->pos = tmp; return 0; }
}

static Node* parse_expr(ParserContext *p, size_t min_bp) {
    Node *left = NULL;
    Node *cast_type;

    if (try_parse_cast(p, cast_type)) {
        Node *n = parse_expr(p, 80);
        left = mk_cast(cast_type, n);
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

static Node *parse_lvalue(ParserContext *p) {
    if (peek(p).type != TOK_IDENT) return NULL;
    Node *n = mk_var(consume(p));
    for (;;) {
        if (peek(p).type == TOK_LBRACKET) {
            consume(p);
            Node *idx = parse_expr(p, 0);
            if (!match(p, TOK_RBRACKET)) return NULL;
            n = mk_index(n, idx);
            if (peek(p).type == TOK_LBRACKET) return NULL;
        } else if (peek(p).type == TOK_DOT && peek_next(p).type == TOK_IDENT) {
            consume(p);
            n = mk_field(n, consume(p));
        } else break;
    }
    return n;
}

static int try_parse_assign_expr(ParserContext *p, Node **out) {
    size_t mark = p->pos;
    Node *lvalue = parse_lvalue(p);
    if (!lvalue) { p->pos = mark; return 0; }
    if (!match(p, TOK_EQ)) { p->pos = mark; return 0; }
    Node *rvalue = parse_expr(p, 0);
    
    *out = mk_assign(lvalue, rvalue);
    return 1;
}

static int try_parse_declexpr(ParserContext *p, Node **out) {
    size_t mark = p->pos;
    bool committed = false;
    bool is_const = false;
    Node *n = NULL;
    
    if (match(p, TOK_KW_CONST)) { is_const = true; committed = true; }
    Token name = {0};

    if (is_bitype(peek(p))) {
        committed = true;
        n = mk_var(consume(p));
        if (peek(p).type == TOK_LBRACKET) {
            consume(p);
            Node *idx = parse_expr(p, 0);
            expect_sync(p, TOK_RBRACKET, SYNC_BRACKET);
            n = mk_index(n, idx);
        }
        Token tmp = peek(p);
        if (tmp.type == TOK_LBRACKET) error_tok(p, tmp, "multi-demensional arrays are not supported");
        else if (tmp.type == TOK_IDENT) name = consume(p);
        else error_tok(p, tmp, "expected name after type");
    } else if (peek(p).type == TOK_IDENT) {
        n = mk_var(consume(p));
        if (peek(p).type == TOK_LBRACKET) {
            consume(p);
            Node *idx = parse_expr(p, 0);
            if (!match(p, TOK_RBRACKET)) { p->pos = mark; return 0; }
            n = mk_index(n, idx);
        }
        Token tmp = peek(p);
        if (tmp.type == TOK_LBRACKET) { p->pos = mark; return 0; }
        else if (tmp.type != TOK_IDENT) {
            if (!committed) { p->pos = mark; return 0; }
            error_tok(p, tmp, "expected name after type");
        } else name = consume(p);
    } else if (is_const) {
        error_tok(p, peek(p), "expected type after const");
    } else { p->pos = mark; return 0; }
        
    Node *init = NULL;
    if (match(p, TOK_EQ)) { init = parse_expr(p, 0); }
    
    *out = mk_declexpr(is_const, n, name, init);
    return 1;
}

static inline void init(NodeList *l, ParserContext *p) {
    Node *n = NULL;
    if (try_parse_declexpr(p, &n)) {
        push(l, n);
    } else if (try_parse_assign_expr(p, &n)) {
        push(l, n);
    } else {
        error_tok(p, peek(p), "expect declaration expression or assign expression");
        sync(p, SYNC_EXPR);
    }
}

static NodeList forinit(ParserContext *p) {
    NodeList list = (NodeList){NULL, 0, 0};
    init(&list, p);

    while (peek(p).type == TOK_COMMA) { 
        consume(p);
        init(&list, p);
    }

    return list;
}

static inline void step(NodeList *l, ParserContext *p) {
    Node *n = NULL;
    if (try_parse_assign_expr(p, &n)) { push(l, n); }
    else { push(l, parse_expr(p, 0)); }
}

static NodeList forstep(ParserContext *p) {
    NodeList list = (NodeList){NULL, 0, 0};
    step(&list, p);

    while (peek(p).type == TOK_COMMA) {
        consume(p);
        step(&list, p);
    }

    return list;
} 

static Node *parse_block(ParserContext *p);

static Node *diispecherzadach(ParserContext *p) {
    switch (peek(p).type) {
        case TOK_LBRACE: return parse_block(p);
        case TOK_SEMICOLON: consume(p); return NULL;
        case TOK_KW_BREAK: consume(p); expect_sync(p, TOK_SEMICOLON, SYNC_STMT); return mk(NK_BREAK);
        case TOK_KW_RETURN:{
            consume(p);
            Node *n = NULL;
            if (peek(p).type != TOK_SEMICOLON) n = parse_expr(p, 0);
            expect_sync(p, TOK_SEMICOLON, SYNC_STMT);
            return mk_return(n);
        }
        case TOK_KW_WHILE:{
            consume(p);
            if (!expect_sync(p, TOK_LPAREN, SYNC_PAREN)) return mk_error("while err");
            Node *c = parse_expr(p, 0);
            expect_sync(p, TOK_RPAREN, SYNC_PAREN);
            return mk_while(c, diispecherzadach(p));
        }
        case TOK_KW_IF:{
            consume(p);
            if (!expect_sync(p, TOK_LPAREN, SYNC_PAREN)) return mk_error("if err");
            Node *c = parse_expr(p, 0);
            expect_sync(p, TOK_RPAREN, SYNC_PAREN);
            Node *n = mk_if(c, diispecherzadach(p));
            if (match(p, TOK_KW_ELSE)) n->if_stmt.else_chain = diispecherzadach(p);
            return n;
        }
        case TOK_KW_FOR: {
            consume(p);
            if (!expect_sync(p, TOK_LPAREN, SYNC_PAREN)) return mk_error("for err");

            NodeList fi = {0};
            Node *cond = NULL;
            NodeList st = {0};
            if (peek(p).type == TOK_RPAREN) 
                error_tok(p, consume(p), "did you mean ';;' for an empty for loop initializer?");
            else {
                if (!match(p, TOK_SEMICOLON)) { fi = forinit(p); expect_sync(p, TOK_SEMICOLON, SYNC_PAREN); }
                if (peek(p).type == TOK_RPAREN) error_tok(p, peek(p), "expect expresion and/or TOK_SEMICOLON");
                else if (!match(p, TOK_SEMICOLON)) { cond = parse_expr(p, 0); expect_sync(p, TOK_SEMICOLON, SYNC_PAREN); }
                if (!match(p, TOK_RPAREN)) { st = forstep(p); expect_sync(p, TOK_RPAREN, SYNC_PAREN); }
            }
            Node *body = diispecherzadach(p);
            return mk_for(fi, cond, st, body);
        }
        default: {
            size_t err0 = p->err_count;

            Node *decl = NULL;
            if (try_parse_declexpr(p, &decl)) {
                expect_sync(p, TOK_SEMICOLON, SYNC_STMT);
                if (p->err_count > err0) return mk_error("declexpr err");
                return decl;
            }

            Node *asg = NULL;
            if (try_parse_assign_expr(p, &asg)) {
                expect_sync(p, TOK_SEMICOLON, SYNC_STMT);
                if (p->err_count > err0) return mk_error("assign expr err");
                return asg;
            }

            Node *e = parse_expr(p, 0);
            expect_sync(p, TOK_SEMICOLON, SYNC_STMT);
            if (p->err_count > err0) return mk_error("exprstmt err");

            return mk_expr(e);
        }
    }
}

static Node *parse_block(ParserContext *p) {
    expect_sync(p, TOK_LBRACE, SYNC_STMT);
    NodeList stmt_list = (NodeList){0};

    while (peek(p).type != TOK_RBRACE && peek(p).type != TOK_EOF) {
        Node *stmt = diispecherzadach(p);
        push(&stmt_list, stmt);
    }

    expect_sync(p, TOK_RBRACE, SYNC_BRACE);
    return mk_block(stmt_list);
}


static NodeList parse_param(ParserContext *p) {
    NodeList list = (NodeList){NULL, 0, 0};
    while (peek(p).type == TOK_IDENT) {
        Token t = consume(p);
        Node *p_type = NULL;
        expect_sync(p, TOK_COLON, SYNC_PAREN);

        if (is_bitype(peek(p)) || peek(p).type == TOK_IDENT) {
        p_type = mk_var(consume(p));
            if (peek(p).type == TOK_LBRACKET && peek_next(p).type == TOK_RBRACKET) {
                consume(p);
                p_type = mk_index(p_type, mk_number(-1, consume(p)));
                if (peek(p).type == TOK_LBRACKET) {
                    error_tok(p, peek(p), "multi-demensional arrays are not supported");
                    sync(p, SYNC_PAREN);
                    return list;
                }
            }
        } else {
            error_tok(p, t, "expected type");
            sync(p, SYNC_PAREN);
            return list;
        }

        push(&list, mk_param(t, p_type));
        if (match(p, TOK_COMMA)) { 
            if (peek(p).type != TOK_IDENT) error_tok(p, peek(p), "trailing comma");
        } else break;
    }
    return list;
}

static Node *parse_fn(ParserContext *p) {
    Token t = peek(p);
    Node *ret_type = NULL;

    if (is_bitype(t) || t.type == TOK_IDENT || t.type == TOK_KW_VOLOID) {
        ret_type = mk_var(consume(p));
        if (peek(p).type == TOK_LBRACKET && peek_next(p).type == TOK_RBRACKET) {
            consume(p);
            ret_type = mk_index(ret_type, mk_number(-1, consume(p)));
            if (peek(p).type == TOK_LBRACKET) {
                error_tok(p, peek(p), "multi-demensional arrays are not supported");
                sync(p, SYNC_BLOCK);
                Node *stmt = diispecherzadach(p);
                return mk_error("fn err");
            }
        }
    } else {
        error_tok(p, t, "expected return type");
        sync(p, SYNC_BLOCK);
        Node *stmt = diispecherzadach(p);
        return mk_error("fn err");
    }

    Token name = (Token){0};

    if (peek(p).type != TOK_IDENT) { error_tok(p, peek(p), "expected name"); }
    else name = consume(p);

    expect_sync(p, TOK_LPAREN, SYNC_PAREN);
    NodeList list = parse_param(p);
    expect_sync(p, TOK_RPAREN, SYNC_PAREN);

    Node *stmt = diispecherzadach(p);

    return mk_fn(ret_type, name, list, stmt);
}

static NodeList parse_field(ParserContext *p) {
    NodeList fields = {NULL, 0, 0};
    while (peek(p).type == TOK_IDENT) {
        Node *n = NULL;
        Token name = consume(p);
        expect_sync(p, TOK_COLON, SYNC_BRACE);
        Token t = peek(p);

        if (is_bitype(t) || t.type == TOK_IDENT) {
            n = mk_var(t);
            consume(p);
            if (peek(p).type == TOK_LBRACKET) {
                consume(p);
                n = mk_index(n, parse_expr(p, 0));
                expect_sync(p, TOK_RBRACKET, SYNC_BRACKET);
                if (peek(p).type == TOK_LBRACKET) {
                    error_tok(p, peek(p), "multi-demensional arrays are not supported");
                    sync(p, SYNC_PAREN);
                }
            }
        } else {
            error_tok(p, peek(p), "expected name");
            sync(p, SYNC_PAREN);
        }

        expect_sync(p, TOK_SEMICOLON, SYNC_BRACE);
        push(&fields, mk_stc_field(name, n));
    }
    return fields;
}

static Node *parse_struct(ParserContext *p) {
    Token name = {0};

    if (peek(p).type != TOK_IDENT) {
        error_tok(p, peek(p), "expected name");
        sync(p, SYNC_TOPLEVEL);
        return mk_error("struct err");
    }
    name = consume(p);

    expect_sync(p, TOK_LBRACE, SYNC_BRACE);
    NodeList list = parse_field(p);
    expect_sync(p, TOK_RBRACE, SYNC_BRACE);

    return mk_struct(name, list);
}

static NodeList parse_program(ParserContext *p) {
    NodeList program = (NodeList){0};
    while (peek(p).type != TOK_EOF) {
        if (match(p, TOK_KW_FN)) { push(&program, parse_fn(p)); }
        else if (match(p, TOK_KW_STRUCT)) { push(&program, parse_struct(p)); }
        else {
            error_tok(p, peek(p), "expected 'fn' or 'struct' at toplevel");
            sync(p, SYNC_TOPLEVEL);
            if (peek(p).type == TOK_RBRACE) consume(p);
        }
    }
    return program;
}

static void dump_parser(size_t d, Node *n, ParserContext *p) {
    for (int i = 0; i < d; i++) printf(" ");
    if (!n) { printf("(null)\n"); return; }
    switch (n->kind) {
        case NK_NUMBER: printf("Number(%lld)", n->number.value); break;
        case NK_BOOLEAN: printf("Boolean(%d)", n->boolean.value ? 1 : 0); break;
        case NK_VAR: printf("Var(%.*s)", (int)n->var.length, n->var.start_pos); break;
        case NK_STRING: printf("String(%.*s)", (int)n->string.length, n->string.start_pos); break;
        case NK_CALL:
            printf("call (");
            dump_parser(0, n->call.called, p);
            printf(", [");
            for (int i = 0; i < n->call.args.length; i++) {
                dump_parser(0, n->call.args.data[i], p);
                i == n->call.args.length - 1 ? : printf(", ");
            }
            printf("])"); break;
        case NK_INDEX:
            printf("index (");
            dump_parser(0, n->index_.array, p);
            printf(", ");
            dump_parser(0, n->index_.index, p);
            printf(")"); break;
        case NK_FIELD:
            printf("field (");
            dump_parser(0, n->field.strc, p);
            printf(", ");
            dump_parser(0, n->field.var, p);
            printf(")"); break;
        case NK_BINARY:
            printf("binary ('%.*s'\n", (int)n->binary.op.length, n->binary.op.start_pos);
            dump_parser(d + 1, n->binary.lhs, p);
            printf("\n");
            dump_parser(d + 1, n->binary.rhs, p);
            printf("\n)"); break;
        case NK_UNARY:
            printf("unary ('%.*s'\n", (int)n->unary.op.length, n->unary.op.start_pos);
            dump_parser(d + 1, n->unary.expr, p);
            printf("\n)"); break;
        case NK_CAST:
            printf("cast (type:'");
            dump_parser(0, n->cast.cast_type, p);
            dump_parser(d + 1, n->cast.expr, p);
            printf("\n)"); break;
        case NK_ARRAY_LIT:
            printf("array literal [");
            for (int i = 0; i < n->array_lit.length; i++) {
                dump_parser(0, n->array_lit.data[i], p);
                i == n->array_lit.length - 1 ? : printf(", ");
            }
            printf("]\n"); break;
        case NK_ERROR: printf("%s", n->error); break;
        case NK_RETURN: printf("return"); dump_parser(0, n->return_stmt, p); break;
        case NK_WHILE: printf("while (cond: ");
            dump_parser(0, n->while_loop.cond, p);
            printf(")\nstmt: ");
            dump_parser(0, n->while_loop.stmt, p); break;
        case NK_BREAK: printf("break\n"); break;
        case NK_IF: printf("if (cond: ");
            dump_parser(0, n->if_stmt.cond, p);
            printf(")\nstmt: ");
            dump_parser(0, n->if_stmt.stmt, p); 
            printf("\nelse ");
            dump_parser(0, n->if_stmt.else_chain, p); break;
        case NK_ASSIGN:
            printf("lvalue: ");
            dump_parser(d, n->assign.lvalue, p);
            printf(" value: ");
            dump_parser(d, n->assign.rvalue, p); break;
        case NK_DECLEXPR:
            printf("const: %d ", n->declexpr.is_const);
            printf("type: ");
            dump_parser(0, n->declexpr.type, p);
            printf(" name: %.*s", (int)n->declexpr.name.length, n->declexpr.name.start_pos);
            printf(" value: ");
            dump_parser(d, n->declexpr.rvalue, p); printf("\n"); break;
        case NK_EXPR: printf("expr_stmt: "); dump_parser(d, n->expr_stmt, p); break;
        case NK_BLOCK: printf("{\n");
            for (size_t i = 0; i < n->stmts.length; i++) {
                dump_parser(0, n->stmts.data[i], p);
                i == n->stmts.length - 1 ? : printf("\n");
            }
            printf("\n}\n"); break;
        case NK_FOR:
            printf("forinit:\n");
            for (int i = 0; i < n->for_stmt.init.length; i++) {
                dump_parser(0, n->for_stmt.init.data[i], p);
                i == n->for_stmt.init.length - 1 ? : printf("\n");
            }
            printf("\ncond:\n");
            dump_parser(0, n->for_stmt.expr, p);
            printf("\nforstep:\n");
            for (int i = 0; i < n->for_stmt.step.length; i++) {
                dump_parser(0, n->for_stmt.step.data[i], p);
                i == n->for_stmt.step.length - 1 ? : printf("\n");
            }
            printf("\nstmt:\n");
            dump_parser(0, n->for_stmt.stmt, p); break;
        case NK_PARAM:
            printf("param: ");
            dump_parser(0, n->param.p_type, p);
            printf(" name: %.*s", (int)n->param.name.length, n->param.name.start_pos);
            break;
        case NK_FN:
            printf("\nfn return: ");
            dump_parser(0, n->fn.ret_type, p);
            printf(" name: %.*s\n", (int)n->fn.name.length, n->fn.name.start_pos);
            for (int i = 0; i < n->fn.params.length; i++) {
                dump_parser(0, n->fn.params.data[i], p);
                printf("\n");
            }
            dump_parser(0, n->fn.stmt, p); break;
        case NK_STC_FIELD:
            printf("name: %.*s ", (int)n->stc_field.name.length, n->stc_field.name.start_pos);
            printf("field: ");
            dump_parser(0, n->stc_field.type, p); break;
        case NK_STRUCT:
            printf("struct name: %.*s\n", (int)n->struct_.name.length, n->struct_.name.start_pos);
            printf("fields:\n");
            for (int i = 0; i < n->struct_.fields.length; i++) {
                dump_parser(0, n->struct_.fields.data[i], p);
                i == n->struct_.fields.length - 1 ? : printf("\n");
            }
            break;
        case NK_PROGRAM:
            for (int i = 0; i < n->program.length; i++) {
                dump_parser(0, n->program.data[i], p);
                printf("\n");
            }
        default: return;
    }
}

Node *parse(TokenVec vec,const char *path) {
    ParserContext p = (ParserContext){ .file = path, .vec = &vec, .pos = 0 };
    Node *n = mk(NK_PROGRAM);
    n->program = parse_program(&p);
    dump_parser(0, n, &p);
    //if (p.err_count) fprintf(stderr, "errors: %zu\n", p.err_count);
    return n;
}