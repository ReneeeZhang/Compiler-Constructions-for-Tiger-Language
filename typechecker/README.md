# Type Checker, Escape Analysis Documentation
Anshu Dwibhashi, Yuanrui Zhang, Clayton Shafer  
ECE 553 Spring 2021

## Improvement
Instead of having an sml datatype `enventry`, which contains `VarEntry` and `FunEntry`, we set `enventry` as a type alias of `{access: unit, ty: ty}`. To make this update happen, we move the original `FunEntry` to `type.sml` as a new `ty`: `ARROW of ty list * ty`, which expresses a function type: arguments * return.

## Extra Credit: Pure Functional Types
### Introduction
In the original `types.sml`, sml datatype `ty` has ` RECORD of (Symbol.symbol * ty) list * unique` along with `NAME of Symbol.symbol * ty option ref`. This initial setting can facilitate declarations of recursive record types, however, it is not a pure functional style. Therefore, our implementation utilizes lazy evaluzation in functional programming so that we can represent an infinite object in a finite way.

Here are what we have changed in the initial settings:
* Record type: `RECORD of (unit -> (Symbol.symbol * ty) list) * unique`.
* Delete `NAME`. By doing so, we effectively get rid of `ref` to achieve pure functional-programming purpose.

### Implementations
To translate each type declaration, we pass to `transTy` function the following arguments in order:
* type environment: `tenv`
* type name: `name`
* a map that accosicates a type name to its unique reference: `unique_ref_map`
* a type declaration group: `tydec_group` (note that Tiger language: allows adjacent type declarations as mutually recursive, so that they are represented as a list in AST)
* a type node: `ty` (`Absyn.NameTy`, `Absyn.ArrayTy` or `Absyn.RecordTy`)

If `ty` is pattern matched as `RecordTy(fields)`, we generate a thunk: 
```
fn() => map (fn {name, typ, ...} => (name, proc(typ))) fields
```
where `proc` is a recursive function to address translation. Because of lazy evaluation, `proc` won't be called endlessly even if it is translating a recursive record type.

`proc` takes a type name (`type_sym`) as argument. It will try to look for it in the type declaration group. If it is there, a mutually recursive call to `address` starts; otherwise, it will search in the tenv.

`address` takes as arguments a type name and a type node in AST. It basicall pattern matches the type node and recursively call `proc`.

Note that for record types, apart from the thunk, we also need to figure out which unique reference it should be associated with. If the type name appears in the unique reference map, whatever in the map will be given; otherwise, a new reference is assigned.