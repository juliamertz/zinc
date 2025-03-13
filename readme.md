# My unfinished language

## Todo

- [ ] Expand on pattern parsing
- [ ] Implement match expression evaluation
- [ ] expand binary expression evaluation
- [ ] Implement for/while loops
- [x] Implement structs/objects
- [x] operator precedence
- [x] make semicolons optional
- [x] Equality operator parsing / eval

### Operator precedence

I started of by following the [interpreter book]() by Thorsten Ball but i quickly got excited and started writing the rest on my own.
This went well for a good amount of time until i stumbled upon operator precedence parsing which had me look back to see how it was tackled in the book,
while doing this i realized i accidentally did the right thing by parsing expressions recursively and passing the left side of a binary expression into the parsing function
from here the only thing i had to do was creating a lookup table for precedences and looping until a lower precedence was found, where then it can go back to recursing

### What i learned about zig

Since i didn't have much experience with C before i started this project there was still a lot to learn, most of the low level programming i had done before this was in rust which abstracts a lot of these concepts away.

- Allocations and when to use pointers

