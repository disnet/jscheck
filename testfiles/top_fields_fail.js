//# @type Dog dog 
function getSaying(dog) { 
  dog.meow;
}

function Dog(name) {
  this.name = name;
}

Dog.prototype.getName = function() {
  return this.name;
}

Dog.prototype.bark = function() {
  return "Arf!";
}

var mydog = new Dog("Spot");
getSaying(mydog);
