#pragma once
#include <stddef.h>
#include "Token.h"

typedef struct {
    Token *data;
    size_t cap;
    size_t count;
} TokenVec;