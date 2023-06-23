function Animal(name) {
  console.log("Animal constructor called with:", name);
  this.name = name;
}

Animal.prototype.sayName = function () {
  console.log("My name is", this.name);
};

function Dog(name, breed) {
  Animal.call(this, name);
  console.log("Dog constructor called with:", name, breed);
  this.breed = breed;
}

Dog.prototype = Object.create(Animal.prototype);

Dog.prototype.sayFullName = function () {
  console.log("My name is", this.name, "and I am a", this.breed);
};

let d = new Dog("Fido", "Labrador");
d.sayFullName();
d.sayName();

// TODO: instanceof test
