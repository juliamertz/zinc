#!/usr/bin/env bash

name="${1%.*}"
qbe "$1" | as -o temp.o \
  && musl-gcc -o "$name" temp.o -static \
  && "./$name" \
  && rm temp.o "$name"
