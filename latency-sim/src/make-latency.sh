#!/bin/bash

NAME=Latency
INPUTS=$(cat toc.list)
FORMAT=markdown+tex_math_dollars+yaml_metadata_block+citations
OPTS="--filter=pandoc-citeproc --natbib --from=${FORMAT} --bibliography=Latency.bib --pdf-engine=xelatex"
DOCOPTS='--filter=pandoc-hide-codeblocks '

echo "Making hide-codeblocks filter"
stack install latency-sim:exe:pandoc-hide-codeblocks # for `hide-codeblocks`

echo "Making TEX"
stack exec -- pandoc ${OPTS} ${DOCOPTS}          --variable mainfont="DejaVu Serif" --variable sansfont="DejaVu Sans" --standalone --to=latex ${INPUTS} -o ${NAME}.tex

echo "Making PDF"
stack exec -- pandoc ${OPTS} ${DOCOPTS}          --variable mainfont="DejaVu Serif" --variable sansfont="DejaVu Sans"                         ${INPUTS} -o ${NAME}.pdf

echo "Making .html"
stack exec -- pandoc ${OPTS} ${DOCOPTS} --mathml --variable mainfont="DejaVu Serif" --variable sansfont=Arial                                 ${INPUTS} -o ${NAME}.html --highlight-style=espresso

echo "Building .lhs to check source code is valid"
stack build

echo "Checking for missing files in toc.list:"
for i in *.lhs *.mmd; do grep $i toc.list>/dev/null || echo "Missing file in TOC: $i"; done
for i in ../test/*.lhs; do grep $i toc.list>/dev/null || echo "Missing file in TOC: $i"; done

