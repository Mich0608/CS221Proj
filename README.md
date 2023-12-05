# CS221Proj

### Building the Compiler
In SML, run `CM.make "proj.cm"` to build the compiler.

### L23Proj
`L23Proj` is our attempt to technically engage with the gradual typing system as defined by Siek and Taha in [this paper](http://scheme2006.cs.uchicago.edu/13-siek.pdf).

The term grammar for `L23Proj` is <br>
```
t ::= n             // n is a number 
    | T             // true 
    | F             // false 
    | x             // variables 
    | [lam x:tau t] // typed lambdas 
    | [lam x t]     // untyped lambdas 
    | (t t)         // applications 
```

Note that both **typed** and **untyped** lambdas are allowed.

While the gradually typed language defined by Siek and Taha paper also allow reference types, we are not including them as we have not previously programmed a compiler for reference types, and we feel that the above terms give us enough flexibility for the purpose of experimenting with gradual typing.

### Type Checking
The type checker in `TypeCheck.sml` follows the gradual typing system laid out in **Figure 2** of the Siek and Taha paper. 
There is a separate type system for the intermediate language after cast insertion (defined in **Figure 6** in the paper). This type checker is implemented in `CastedTypeCheck.sml`. 

### Cast Insertion
Cast insertion in `Cast.sml` follows the cast insertion judgements in **Figure 5** of the paper. The result of casting a `L23Proj` term is a `Casted` term that represents a term in the intermediate language.

### Testing
To test cast insertion using the two examples from **Section 5.3** of the the Siek and Taha paper, run `Test.cast()`. You can see the tests in `test.sml`.
