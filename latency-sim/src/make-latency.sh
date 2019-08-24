#!/bin/bash

NAME=Latency
INPUTS=$(cat toc.list)
FORMAT=markdown+tex_math_dollars+yaml_metadata_block+citations
OPTS="--filter=pandoc-citeproc --filter=pandoc-hide-codeblocks --bibliography=Latency.bib --from=${FORMAT} --pdf-engine=xelatex --table-of-contents"
#DOCOPTS='--filter=pandoc-hide-codeblocks '

echo "Making hide-codeblocks filter"
stack build latency-sim:exe:pandoc-hide-codeblocks # for `hide-codeblocks`

echo "Making TEX"
stack exec -- pandoc ${OPTS} ${DOCOPTS}          --variable mainfont="DejaVu Serif" --variable sansfont="Fira Code" --standalone --to=latex ${INPUTS} -o ${NAME}.tex --standalone

echo "Making PDF"
stack exec -- pandoc ${OPTS} ${DOCOPTS}          --variable mainfont="DejaVu Serif" --variable sansfont="Arial"                         ${INPUTS} -o ${NAME}.pdf

echo "Making .html"
stack exec -- pandoc ${OPTS} ${DOCOPTS} --mathml --variable mainfont="DejaVu Serif" --variable sansfont="Arial"                                 ${INPUTS} -o ${NAME}.html --highlight-style=espresso

echo "Building .lhs to check source code is valid"
stack build

echo "Checking for missing files in toc.list:"
for i in *.lhs *.mmd;   do grep $i toc.list>/dev/null || echo "Missing file in TOC: $i"; done
for i in ../test/*.lhs; do grep $i toc.list>/dev/null || echo "Missing file in TOC: $i"; done
for i in ../sim/*.lhs;  do grep $i toc.list>/dev/null || echo "Missing file in TOC: $i"; done

