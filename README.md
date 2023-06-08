# MeguKin

## Description of the Language

`MeguKin` aims to be a functional language with strong type system and 
static type check evolving from simple typed lambada calculus + adt + records
(all monomorphic), to a λω + type classes.

## Motivation

I want to learn about code generation for functional languages and 
others things.

The source of the first compiler and the target of compilation is python 
since I want to be able (more or less) to:
  - Get a functional compiler with ease in any platform.

## How to try

Only syntax is supported by now, but you can try it!

After repo cloning you can install it in editor mode:

```bash
pip install -e .
```

Then you have available the command `megukin`, you can see the help with `megukin -p`. 
There are a couple of rules of the grammar that can be tested here but the only 
at the top of the module right now is `top_variable_definition`, use it like: 

```bash
meguking -s "top_variable_definition" "a=a"
```

It would show you the intermediate parts and even a pretty print!

## RoadMap

- Implement simple typed lambda calculus without syntax sugar, with
  adts and iterpreted in python

- Add compilation to python (maybe add mypy and pypy support here?)

- Add records and Modules

- create a lsp for the language

- Maybe do more backends like C or wasm or even llvm

- Maybe Add indentation

- Add terms depending on types and types depending on types

- Add classes
