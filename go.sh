#!/bin/bash
for i in {0..9}
do
  time stack exec depsolver-exe tests/seen-$i
  echo $i
done
