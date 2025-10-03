#pragma once
#ifndef ARR_H
#define ARR_H

#include <stddef.h>
#include "Token.h"

extern const char *TOKEN_NAMES[TOK__COUNT];
const char *token_type_name(TokenType t);

#endif