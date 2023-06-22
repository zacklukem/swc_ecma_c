let drop_me = "drop me";
drop_me = "bye";

__swcjs__.gc();

function my_function() {
  let words = "hello world";
  console.log(words);
}

function fib_recursive(n) {
  if (n < 2) {
    return 1;
  } else {
    return fib_recursive(n - 1) + fib_recursive(n - 2);
  }
}

console.log("Fib 8 rec:", fib_recursive(8));

my_function();
let a;
let b = 32;
let c = "hi";
console.log(c);
if (c == "asdf") {
  let a = 3;
  a = 2;
  console.log("asdf", 3);
  // myfun(undefined);
} else {
  console.log("not asdfasdf", 5);
}

console.log("Fibonacci:");

let v1 = 1;
let v2 = 1;

for (let i = 0; i < 20; i = i + 1) {
  console.log(i + 1, v2);

  let temp = v2;
  v2 = v1 + v2;
  v1 = temp;
}
