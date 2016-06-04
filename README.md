# Meta II

## Introduction

This is an implementation of Val Schorre's Meta II language in Common
Lisp. Meta II is a compiler-writing language, in which you express the
language of the language using syntax similar to BNF. The input is
compiled into an assembly language. The code here is a virtual machine
for the target language.

Schorre, D.V., 1964. Meta II: a Syntax-oriented Compiler Writing
Language. In Proceedings of the 1964 19th ACM national conference (pp.
D1.3-1 -- D1.3-11).

Other useful references:
* [Online version of the Schorre paper](http://www.chilton-computing.org.uk/acl/literature/reports/p025.htm)
* James Neighbors extremely detailed explication of Meta II: [metacompiler tutorial](http://www.bayfronttechnologies.com/mc_tutorial.html)
* [Wikipedia page on Meta II](https://en.wikipedia.org/wiki/META_II)

## The files

       File | Contents
------------|---------
metaii.lisp | The virtual machine in Common Lisp
aexp.masm   | James Neighbors algebraic expression syntax, compiled by Meta II
metaii.meta | The Meta II interpreter in Meta II
metaii.masm | The Meta II interpreter, compiled by Meta II

## Examples

* A simple example (from James Neighbors) of an algebraic expression
language:

```lisp
(run (build-instructions (parse-file "aexp.meta"))
     "fern:=5+6;ace:=fern*5;waldo:=fern+alpha/-beta^gamma;")
```

*  Compiling the Meta II compiler.

```lisp
(run (build-instructions (parse-file "metaii.meta"))
     (file-as-string "metaii.meta"))
```
* Compiling the Meta II compiler using the compiled Meta II compiler.

```lisp
(run (build-instructions (parse-file "metaii.masm"))
     (convert-single-to-double (file-as-string "metaii.meta")))
```
