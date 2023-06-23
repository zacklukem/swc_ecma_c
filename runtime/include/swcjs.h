#ifndef SWCJS_H
#define SWCJS_H

#include <swcjs_binding.h>

#define SWCJS_NULL ((swcjs_ValueT*)0)
#define SWCJS_UNDEFINED ((swcjs_ValueT*)1)

// cbindgen doesn't generate vardiac (yet)
struct swcjs_ValueT *swcjs_expr_call(const struct swcjs_ValueT *fun, uint16_t argc, ...);
struct swcjs_ValueT *swcjs_expr_new(const struct swcjs_ValueT *fun, uint16_t argc, ...);
struct swcjs_ValueT *swcjs_init_anon_fn(struct swcjs_ValueT *(*fun)(const struct swcjs_ArgsT*), uint16_t argc, ...);
struct swcjs_ValueT *swcjs_expr_init_obj(uint16_t num_props, ...);

#endif