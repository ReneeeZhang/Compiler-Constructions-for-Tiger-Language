# Frame Analysis and IR Documentation
Anshu Dwibhashi, Yuanrui Zhang, Clayton Shafer  
ECE 553 Spring 2021

## Usage
`main.sml` is the driver for running escape analysis and semantic analysis on a particular file. After building, run `Main.run "test.tig"`. This will print all fragments and their corresonding labels. To make the output more readable, run `Main.lin "test.tig"`, which will linearize the fragments using Appel's Chapter 8 file, removing all SEQ and ESEQ constructions, and print the result.
## Extra Credit - Integer Arithmetic
All integer-valued expressions that are composed of multiple constants (even if they have non-constants embedded in them) are aggressively optimized. Our compiler is aggressive in the sense that in an arithmetic term composed of multiple factors (even if some of these factors are non-constant), the compiler evaluates the term statically and replaces all the constants with one constant factor (and any other non-constant factors that may be present in the original expression). Our compiler does so no matter what order the factors appear in (while still respecting order of operation and associativity). Likewise, we also evaluate all constant terms in a expression (even if they are interrupted by some terms which are composed of non-constant factors, which we ignore) and replace all such terms with one constant term and add/subtract the other non-constant terms to this term while preserving the semantic meaning.

Example Tiger program:

```
let
    function convertHoursToSeconds(hours:int) : int =  hours * 60 * 60
in
    convertHoursToSeconds(20)
end
```

IR translation before optimization:

```
tig_main : 
MOVE(
 TEMP t101,
 CALL(
  NAME L1,
   TEMP t100,
   CONST 20))
L1 : 
MOVE(
 TEMP t101,
 BINOP(MUL,
  BINOP(MUL,
   TEMP t106,
   CONST 60),
  CONST 60))
```

IR translation after optimization (note that basic implementations of this optimization will be thrown off by associativity impositions placed by the AST, and won't be able to optimize this unless the non-constans showed up as the right-most factor):
```
tig_main : 
MOVE(
 TEMP t101,
 CALL(
  NAME L1,
   TEMP t100,
   CONST 20))
L1 : 
MOVE(
 TEMP t101,
 BINOP(MUL,
  TEMP t106,
  CONST 3600))
```

Note that if a constant division is found in an expression, our compiler evaluates it if the result is an integer, but ignores a division if the result may or may not be an integer (depending on the non-constant terms' values), because Tiger does not support floating point values.

For example,

```
let
    var x: int := 20 / 2
in
    x
end
```

Is simplified to:
```
tig_main : 
MOVE(
 TEMP t106,
 CONST 10)
MOVE(
 TEMP t101,
 TEMP t106)
```

However,

```
let
    var x: int := 20 / 3
in
    x
end
```

Is not simplified, because if this constant were multiplied by something else that was divisble by 3, we would like to preserve this as-is,

```
tig_main : 
MOVE(
 TEMP t106,
 BINOP(DIV,
  CONST 20,
  CONST 3))
MOVE(
 TEMP t101,
 TEMP t106)
```

However, it is only the problematic factor that is ignored; all other factors are still agressively simplified as in this example:

```
let
    var x: int := 20 / 3 / 10
in
    x
end
```

Which evaluates to:

```
tig_main : 
MOVE(
 TEMP t106,
 BINOP(DIV,
  CONST 2,
  CONST 3))
MOVE(
 TEMP t101,
 TEMP t106)
```

Our compiler can simplify quite complex expressions while avoiding such problematic divisions:

```
let
    var x: int := 20 / 3 / 10 + 3 * 6 / 2 - 12 + 2 * (-2)
in
    x
end
```
Evaluates to:
```
MOVE(
 TEMP t106,
 BINOP(PLUS,
  BINOP(DIV,
   CONST 2,
   CONST 3),
  CONST ~7))
MOVE(
 TEMP t101,
 TEMP t106)
```

Our compiler can work around non-constant factors and terms, and still simplify constants to the greatest extent possible, while still avoiding problematic divisions:

```
let
    var a: int := 1
    var b: int := 1
    var c: int := 2 
    /* Assume the values of a, b and c have changed
        and are unknown during compile time by this line */
    var x: int := 20 / 3 / 10 * a + 3 * 6 / 2 - 12 + 2 * (-2) / b + c
in
    x
end
```
Evaluates to:
```
tig_main : 
MOVE(
 TEMP t106,
 CONST 1)
MOVE(
 TEMP t107,
 CONST 1)
MOVE(
 TEMP t108,
 CONST 2)
MOVE(
 TEMP t109,
 BINOP(PLUS,
  BINOP(MINUS,
   BINOP(PLUS,
    BINOP(MUL,
     BINOP(DIV,
      CONST 2,
      CONST 3),
     TEMP t106),
    CONST ~3),
   BINOP(DIV,
    CONST ~4,
    TEMP t107)),
  TEMP t108))
MOVE(
 TEMP t101,
 TEMP t109)
```
## Extra Credit - If Optimizations

Our compiler is moderately aggressive in detecting and removing redundant if-then or if-then-else expressions. These cases handle cases that may have silly seeming conditions that are found often in generated code as we discussed in class.

An if statement involving a constant is evaluate to a single expression during compile-time:
```
if 0 then 7 else 9
```
Evaluates to:
```
tig_main : 
MOVE(
 TEMP t101,
 CONST 9)
```
Similarly,
```
if 12 then 7 else 9 /* Any non-zero value = true */
```
Evaluates to:
```
tig_main : 
MOVE(
 TEMP t101,
 CONST 7)
End 
```

We also evaluate relational operators that either compare constants, or expressions that evaluate to constants on either side of the relational operator. For example,

```
if 12 < 12 then 7 else 9
```

Evaluates to:
```
tig_main : 
MOVE(
 TEMP t101,
 CONST 9)
```

Whereas,

```
if 12 <= 12 then 7 else 9
```

Evaluates to:

```
tig_main : 
MOVE(
 TEMP t101,
 CONST 7)
```

Our compiler can also evaluate such conditions to integer valued expressions:

```
if 12 <= 12 then 12 <> 24 else 9
```
Evaluates to:
```
tig_main : 
MOVE(
 TEMP t101,
 CONST 1)
```

Finally, we can also evaluate complex arithmetic expressions and remove if statements:
```
if 20 / 10 + 3 * 6 / 2 - 12 + 2 * (-2) <= 0 then -1 else 1
```

Evaluates to:

```
tig_main : 
MOVE(
 TEMP t101,
 CONST ~1)
```
