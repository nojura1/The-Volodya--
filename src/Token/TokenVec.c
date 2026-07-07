#include "TokenVec.h"
#include <stdlib.h>

void tokenvec_free(TokenVec *v) {
    free(v->data);
}