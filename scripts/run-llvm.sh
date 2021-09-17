#!/bin/sh

# Usage: run-llvm JU-FILE [ARG]...

# This wrapper script can be used to directly compile the Juvix program to LLVM
# IR, and execute it using webassembly tools. The result will be printed to
# stdout.
# 
# It requires:
# - llc found in the LLVM distribution.
# - wasm-ld found in the lld package.
# - wasmer

# Cleanup after ourselves.
trap 'cleanup' 0 1 2 3 6 15

# Temporary file for LLVM IR code.
TMPFILE=`mktemp`.ll

cleanup () {
  rm -f $TMPFILE.ll
  rm -f $TMPFILE.o
  rm -f $TMPFILE.wasm
}

#getPath () {
#  progname=$1
#  shift
#  local progpath=$($@)
#  echo $progname
#  if [[ -z "$progpath" ]]; then
#    echo "Unable to find $prog, exiting..." 2>&1
#    exit 1
#  fi
#  echo $progpath
#}

INPUT=$1
shift

if [ -z "$INPUT" ]; then
  echo 'Please provide an INPUT .ju file to compile.'
  exit 1
fi

# The arguments that will be passed to main function.
MAIN_ARGS=$@

# Check for dependencies.
# Prefer LLVM 9 over an unspecified version.
LLC=$(which llc9 || which llc)
if [ "$?" -ne 0 ]; then
  echo 'Unable to find llc, exiting...' 2>&1
  exit 1
fi
WASM_LD=$(which wasm-ld9 || which wasm-ld)
if [ "$?" -ne 0 ]; then
  echo 'Unable to find wasm-ld, exiting...' 2>&1
  exit 1
fi
WASMER=$(which wasmer)
if [ "$?" -ne 0 ]; then
  echo 'Unable to find wasmer, exiting...' 2>&1
  exit 1
fi

# Run the compiler and pass the code to lli. The output of the program is the
# return value of lli, so we need to print it afterwards.
stack run -- compile -b llvm "$INPUT" $TMPFILE.ll
if [ "$?" -eq 0 ]; then
  $LLC -mtriple=wasm64-unknown-unknown -filetype=obj $TMPFILE.ll -o $TMPFILE.o &&
  $WASM_LD $TMPFILE.o -o $TMPFILE.wasm --export=main --no-entry &&
  $WASMER -i main $TMPFILE.wasm $MAIN_ARGS
fi
