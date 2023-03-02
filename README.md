# Use Haskell to implement a Compiler

This is a course project of UBC CPSC 312

it's inspired by the [kaleidoscope](https://llvm.org/docs/tutorial/) project and its [Haskell Version](https://www.stephendiehl.com/llvm/)

kaleidoscope is a extremely simple programming language designed for people to learn LLVM

# Dependencies
there are three main dependencies

## 1. parsec
use `stack install parsec` to install `parsec` package

## 2. llvm-hs
llvm-hs has not been maintained for a long time, and the newest version doesn't compile, so you need
to download them from github and checkout to patched version like: https://github.com/christianlavoie/llvm-hs.git

## 3. bytestring
llvm-hs uses `byteString`, not `String` to improve its performance, but they didn't encapsulate it well, so we are forced to installed bytestring package to do the conversion

# How to run it

1. No input 
  REPL(Read-Eval-Print Loop) Mode

2. Flags
  -i read file
  -o output file
