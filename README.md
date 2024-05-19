# Plutom Programming Language
Plutom is a dynamically-typed, compiled, all-purpose programming language designed to provide flexibility and performance for a wide range of applications. Whether you're developing web applications, system utilities, or anything in between, Plutom aims to be your go-to language for rapid development and efficient execution.

## Features

- **Dynamic Typing**: Enjoy the flexibility of dynamic typing, allowing you to write more expressive and concise code without worrying about type annotations.
- **Compiled Performance**: Benefit from the speed of a compiled language, with optimizations that ensure your code runs quickly and efficiently.
- **All-Purpose Design**: Plutom is designed to be versatile, suitable for a wide range of programming tasks from web development to system scripting.
- **Easy-to-Learn Syntax**: The syntax of Plutom is designed to be easy to learn and use, with a focus on readability and simplicity.

## Getting Started

### Installation

To install Plutom, follow these steps:

1. **Clone the Repository**:
    ```
    git clone https:#github.com/yourusername/plutom.git
    cd plutom
    ```

2. **Build the Compiler**:
    ```
    cargo build --release
    ```

3. **Install the Compiler**:
    ```
    cargo install --path .
    ```

### Writing Your First Program
Create a new file called `hello.plu` with the following content:
```
print("Hello, World!");
```
To compile and run your program, use the Plutom compiler:
```
plutom hello.plu
```
Plutom comes with an interactive REPL (Read-Eval-Print Loop) for experimenting and testing code snippets. To start the REPL, simply run:
```
plutom
```

## Documentation
TODO: Comprehensive documentation for Plutom is available in the docs directory. This includes a language reference, tutorials, and examples to help you get the most out of Plutom.

### Operators
```
# Logic operations

let a = 1 >= 2;     
let b = 3 <= 4;
let c = 5 == 6;     
let d = 7 != 8;     
let e = !(9 < 10);  # Logic negation

TODO: # Ternary
let foo = 1 + 1 == 2 ? true : false;

# Arithmetic operations
let x = 1 + 1;      
let x = 1 - 1;  
let y = -x;         # Negation

TODO: # Prefix incerement
let z = ++y; 

TODO: # Postfix incerement
let z = y++;      

TODO # assignment operators
let a += 1;         
let a -= 1;
let a *= 1;
let a /= 1;

```

---
### Variables
```
let a = 1;
echo a;
```
---
### Functions
```
fn foo(bar, baz) {
    return bar + baz;
}

foo(1, 3);
```
---
### TODO Closures
```
fn outer() {
    let x = 1;
    fn inner() {
        print x;
    }
    return inner();
}

let foo = outer();
foo();
```
---
### TODO Anonymous Function
```
let foo = fn (a, b) {
    return a + b;
};

print(foo(1, 2)); # prints 3
```
---

### List
```
let l1 = [];         # empty list

let l2 = [1, 2, 3];  # initilize list with values
print(l2[0]);        # access item at index   

l2[0] = 2;           # set Value at index
print(l2);
```
---

### Map
```
let m1 = {};         # empty Map

let m2 = {           # initilize Map with values
    1: "key", 
    2: "key2",
    3: "key3" 
};  

print(m2[1]);        # access item with key  

m1["new_key"] = 10;  # insert new Key and Value
m2[3] = "changed";   # update value at Key

print(m1);
print(m2);
```
---

### TODO Class
```
class Animal {
    # Constructor
    init(species) {
        self.species = species;
    }

    # Methods
    getSpecies() {
        return self.species;
    }

    setSpecies(newSpecies) {
        self.species = newSpecies;
    }
}

let dog = Animal("Canis");
print(dog.getSpecies());

```
---
### TODO Inheritance
```
class Animal {
    # Constructor
    init(species) {
        self.species = species;
    }

    # Methods
    getSpecies() {
        return self.species;
    }

    setSpecies(newSpecies) {
        self.species = newSpecies;
    }
}

class Dog<Animal> {
    getSpecies() {
        print("this dog belongs to " + super::get_species()); 
    }
    makeSound() {
        print("baaawrk baawrk#!");
    }
}

let dog = Dog();
print(dog.getSpecies());
dog.makeSound();
```

---
### Comments
```
# Comment
```
---

### Builtins
```
let now = time());  # returns current time in millisecons

let i = 1;
type(i);            # prints Type of the value

sleep(10);          # sleeps for set time in seconds
sleep(1.2);

exit();             # Exit programm
exit(1);            # Set i32 exit code

# print to stdout
print("Hello World");

# read user input from stdin
let awnser = input("How's your day? ");
print(answer);
```
---

### Contributing
Contributions are welcome! If you'd like to contribute to Plutom, please follow these steps:

Fork the Repository: Click the "Fork" button at the top right of the repository page.
Clone Your Fork:
```
git clone https:#github.com/yourusername/plutom.git
cd plutom
```
Create a Branch:
```
git checkout -b feature/your-feature-name
```
Make Your Changes: Implement your changes and add tests as needed.
Commit Your Changes:
```
git commit -m "Add some feature"
```
Push to Your Fork:
```
git push origin feature/your-feature-name
```
Create a Pull Request: Open a pull request on the original repository.

### License
Plutom is licensed under the MIT License. See the LICENSE file for more details.

### Contact
For questions or feedback, feel free to open an issue.
Thank you for using Plutom! Happy coding!
