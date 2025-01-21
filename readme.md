# My unnamed and unfinished language

## Known bugs
- The first statement in a file will fail to parse if there is missing whitespace e.g. `let val=10` will fail but `let val = 10` works fine, this is probably caused by the parsers initialization step where it advances twice.

### A file is a module

Each file should be considered it's own module like zig does it

### It's just an expression bro

Everything that is NOT a statement should be considered an expression,
that means that if/else statements can be directly used inside a variable etc.

### No null values

If i find the motivation i want to add a typesystem someday and banish null values
in favor of options/results rust style

This atrocity exists thanks to https://interpreterbook.com/

