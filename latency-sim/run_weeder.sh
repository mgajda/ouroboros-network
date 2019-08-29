#!/bin/env bash

#shopt -s extglob

mkdir -p src-weeder sim-weeder test-weeder;

unliterate() {
  echo "From $1 to $2";
  for input in $1/*.lhs; do
    markdown-unlit -h `basename $input .lhs` $input $2/`basename $input .lhs`.hs;
  done
  if [[ "$1/*" =~ "*.hs" ]]; then
    for input in $1/*.hs; do
      cp $input $2/`basename $input`
    done
  fi;
}
unliterate src  src-weeder
unliterate test test-weeder
unliterate sim  sim-weeder

stack build

#weeder --build --test
