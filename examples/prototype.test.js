function MyConstructor(input) {
  this.a = "Something";
  this.b = "something else";
  this.input = input;
}

MyConstructor.prototype.printMe = function () {
  console.log("a:", this.a);
  console.log("b:", this.b);
  console.log("input:", this.input);
};

let instance1 = new MyConstructor("instance1");
instance1.printMe();

let instance2 = new MyConstructor("instance2");
instance2.printMe();
