function my_function() {
  console.log("hello world");
}

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

let i = 0;
while (i < 20) {
  console.log(v2);

  let temp = v2;
  v2 = v1 + v2;
  v1 = temp;

  i = i + 1;
}
