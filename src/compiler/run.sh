#!/usr/bin/env bash

name="${1%.*}"
qbe "$name.il" > "$name.asm"
as -o "$name.o" "$name.asm"                  
musl-gcc -o "$name" "$name.o" -static    
"./$name"
rm "$name.asm" "$name.o" "$name"
