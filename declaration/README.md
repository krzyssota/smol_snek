# SMOL SNEK
Smol Snek is a small python-like language with exception of "{}" guarded blocks
(because I personally don't like indents) and semicolon terminating expressions (because BNFC doesn't allow for white-space terminating tokens).
## Syntax/features
* Program is a collection of statements terminated with ";".
* It expression oriented so assignments evaluate (enabling lines like "a += b = 1") and ternary operator is present.
* While and range loops are present (with break and continue), as well as if|esif|else.
* Standard operators are present and there are 3 types available - string, bool, int.
* Comments are prefaced with "#".
* Nested function declaration shadowing and static binding.
* Run-time error handling.

I expect 23 points from this implementation.
