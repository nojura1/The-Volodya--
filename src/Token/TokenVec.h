#pragma once
#include "Token.h"

typedef struct {
    Token *data;
    size_t cap;
    size_t count;
} TokenVec;

void tokenvec_free(TokenVec *v);