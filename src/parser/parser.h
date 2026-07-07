#pragma once
#include "../Token/TokenVec.h"
#include "ast.h"

Node *parse(TokenVec vec, const char *path);