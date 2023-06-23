function outer() {
  let x = 1;
  let f1 = function () {
    let y = 2;
    let add_z = function (z) {
      console.log("HI");
      return x + y + z;
    };
    return add_z(3);
  };
  return f1();
}

console.assert(outer() == 6);

console.log(outer());
