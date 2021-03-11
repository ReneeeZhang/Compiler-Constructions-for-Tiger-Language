# Type Checker, Escape Analysis Documentation
Anshu Dwibhashi, Yuanrui Zhang, Clayton Shafer  
ECE 553 Spring 2021

## Usage
`main.sml` is the driver for running escape analysis and semantic analysis on a particular file. After building, run `Main.run "test.tig"`.

## Break Expressions
To allow our type checker to properly determine the legality of break expressions, we added a unit option to the transExp function. When transExp is called on the body of a loop, it is passed `()`. In all other cases, it is passed `NONE`. When a break expression is encountered, this value is used to determine whether or not the break is inside a loop. 

## Escape Analysis
Before type checking, we have implemented escape analysis, which traverses the AST and updates the escape bool ref to reflect whether or not a particulara variable escapes. This can be found in `escape.sml`.

## Extra Credit
Our typechecker has purely function records ...
