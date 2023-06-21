#ifndef SWCJS_H
#define SWCJS_H

#include <swcjs_binding.h>

#define SWCJS_NULL ((swcjs_ValueT*)0)
#define SWCJS_UNDEFINED ((swcjs_ValueT*)1)

struct swcjs_ValueT *swcjs_expr_call(const struct swcjs_ValueT *fun, uint16_t argc, ...);

#endif