function Dog(name) {
  this.name = name;
}

Dog.prototype.getName = function() {
  return this.name;
}

Dog.prototype.bark = function() {
  return "Arf!";
}

//# @type Dog dog 
function getSaying(dog) { 
  dog.bark();
  dog.meow();
}

var mydog = new Dog("Spot");
getSaying(mydog);
