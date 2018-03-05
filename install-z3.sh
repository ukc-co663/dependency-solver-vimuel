#!/bin/bash

if [ "$(z3 -version)" != "Z3 version 4.6.1 - 64 bit" ]; then
  git clone https://github.com/Z3Prover/z3.git
  cd z3
  python scripts/mk_make.py
  cd build
  make
  make install
fi
