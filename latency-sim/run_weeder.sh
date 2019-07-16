#!/bin/env bash

rm src/*.lhs
(cd src;
for i in *.md; do NAME=$(basename $i .md); markdown-unlit -h $NAME.hs $i $NAME.hs; done)

weeder --build --test
