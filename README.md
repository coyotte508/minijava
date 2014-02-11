The minijavac compiler.
-----------------------

[![Build Status](https://travis-ci.org/coyotte508/minijava.png)](https://travis-ci.org/coyotte508/minijava)

* `ocamlbuild Main.byte` (or native) to build the compiler.
* `ocamlbuild Main.byte -- filename`  to build and then execute the compiler on the given file. `-v` option to also see the converted code.
* `./Tests/run-tests.sh` to run the tests (build in native first).


Langage Syntax
==============

minijava is a bit like Java. Except...

* Simplified syntax (no loops, interfaces, nested classes, arrays)
* Functions can be defined outside classes, code can execute toplevel. 
* Primitive types are Int (`1`), String (`"3"`), Bool (`false`), Void (`()`), they can't be created with `new`, 
 and assume a default value when undefined or set to `null`
* No `return` keyword, last expression is the return value. If a return value of `Void` is expected, the last expression can have any type.
* Cast has +++ priority, `(A)b.x` is the same as `((A)b).x`
* `this` is mandatory when calling up a member variable / function
* Strings can be delimited by simple or double quotes
* two built-ins : `Void print_int(Int)` and `Void print_string(String)`
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

Finished. All's left is improvements (like lifting the restriction on this)
