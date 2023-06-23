__swcjs__.gc_enable_logging();
function dont_drop() {
  return "dont drop me";
}

let drop_me = {
  a: "drop me a",
  b: "drop me b",
};

drop_me = dont_drop();

__swcjs__.gc();
__swcjs__.gc_disable_logging();

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

function print_this() {
  console.log(this);
}

print_this.bind("Printing this!")();

let my_obj = {
  a: "my object has props!",
  b: 2,
};

console.log(my_obj.a);

my_obj.b = "my props change!";

console.log(my_obj.b);

my_obj.c = "my obj has more props!";

console.log(my_obj.c);

function MyConstructor(input) {
  this.a = "Something";
  this.b = "something else";
  this.input = input;
}

let my_obj2 = new MyConstructor("my input");

console.log(my_obj2.input);

console.log("Fib 8 rec:", fib_recursive(8));

let obj = new Object();
obj.a = "constructed object";
console.log(obj.a);

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

console.assert(1 + 1 == 2, "math stuff");
