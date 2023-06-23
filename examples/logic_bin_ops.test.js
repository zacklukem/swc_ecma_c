// TODO: implement this using closures
let state = { pass: 1, fail: 1 };

let fail = function (v) {
  console.log("FAIL", state.fail);
  state.fail = state.fail + 1;
  console.assert(false, "FAIL INNER");
  return v;
};

let pass = function (v) {
  console.log("OK", state.pass);
  state.pass = state.pass + 1;
  return v;
};

if (true && pass(false)) fail();
if (false && fail(false)) fail();
if (false && fail(true)) fail();
if (true && pass(true)) pass();

if (true || fail(false)) pass();
if (false || pass(false)) fail();
if (false || pass(true)) pass();
if (true || fail(true)) pass();

// 8 passes
