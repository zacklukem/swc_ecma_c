function dont_drop() {
  return "dont drop me";
}

let drop_me = {
  a: "drop me a",
  b: "drop me b",
};

let drop_me_saved = __swcjs__.gc_store_ptr(drop_me);
let drop_me_a_saved = __swcjs__.gc_store_ptr(drop_me.a);
let drop_me_b_saved = __swcjs__.gc_store_ptr(drop_me.b);

drop_me = dont_drop();

let drop_me_new_saved = __swcjs__.gc_store_ptr(drop_me);

__swcjs__.gc();

__swcjs__.gc_assert_freed(drop_me_saved);
__swcjs__.gc_assert_freed(drop_me_a_saved);
__swcjs__.gc_assert_freed(drop_me_b_saved);
__swcjs__.gc_assert_saved(drop_me_new_saved);

console.log("OK");
