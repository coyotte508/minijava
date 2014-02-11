The minijavac compiler.
-----------------------

[![Build Status](https://travis-ci.org/coyotte508/minijava.png)](https://travis-ci.org/coyotte508/minijava)

* `ocamlbuild Main.byte` (or native) to build the compiler.
* `ocamlbuild Main.byte -- filename`  to build and then execute the compiler on the given file.
* `./Tests/run-tests.sh` to run the tests (build in native first).


Langage Syntax
==============

minijava is a bit like Java. Except...

* Simplified syntax (no loops, interfaces, nested classes, arrays)
* Functions can be defined outside classes, code can execute toplevel. 
* No `return` keyword, last expression is the return value
* Cast has +++ priority, `(A)b.x` is the same as `((A)b).x`
* `this` is mandatory when calling up a member variable / function
* Parentheses have their natural role, but they can also act as `begin` / `end`

For example, here's how the parentheses can work:
```java
Int y = 1 + 
  (
    /* Can do here everything we could in a function body */
  	Int x = 2;
  	x = x * 10;
  	x - 1;
  );
```

Progress
========

Only up to the AST at the moment.
