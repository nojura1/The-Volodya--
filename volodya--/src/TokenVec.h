#pragma once
#include <stddef.h>
#include "Token.h"

typedef struct {
    Token *data;
    size_t cap;
    size_t count;
} TokenVec;

void tokenvec_init(TokenVec *v);
void tokenvec_push(TokenVec *v, Token t);