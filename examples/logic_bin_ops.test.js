function fail_inner(v) {
  console.log("FAIL", this.fail);
  this.fail = this.fail + 1;
  return v;
}

function pass_inner(v) {
  console.log("OK", this.pass);
  this.pass = this.pass + 1;
  return v;
}

// TODO: implement this using closures
let state = { pass: 1, fail: 1 };
let pass = pass_inner.bind(state);
let fail = fail_inner.bind(state);

if (true && pass(false)) fail();
if (false && fail(false)) fail();
if (false && fail(true)) fail();
if (true && pass(true)) pass();

if (true || fail(false)) pass();
if (false || pass(false)) fail();
if (false || pass(true)) pass();
if (true || fail(true)) pass();

// 8 passes
