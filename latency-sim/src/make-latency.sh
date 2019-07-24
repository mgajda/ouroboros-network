#!/bin/bash

NAME=Latency
INPUTS=$(cat toc.list)
FORMAT=markdown+tex_math_dollars+yaml_metadata_block+citations
OPTS

#echo "Linking diagrams"
#ln -s ../doc/completion-rate.png .
#ln -s ../doc/latency-distribution.png .

echo "Making hide-codeblocks filter"
#ghc hide-codeblocks.hs -o hide-codeblocks
stack install latency-sim:exe:pandoc-hide-codeblocks # for `hide-codeblocks`

echo "Making TEX"
stack exec -- pandoc --filter=pandoc-citeproc --filter=pandoc-hide-codeblocks --natbib --from=${FORMAT} --standalone --variable mainfont="DejaVu Serif" --variable sansfont="DejaVu Sans" --to=latex --pdf-engine=xelatex ${INPUTS} -o ${NAME}.tex --bibliography=Latency.bib
#--filter=pandoc-citeproc --biblio=Latency.bib

echo "Making PDF"
stack exec -- pandoc --filter=pandoc-hide-codeblocks --filter=pandoc-citeproc          --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable  sansfont="DejaVu Sans" --pdf-engine=xelatex ${INPUTS} -o ${NAME}.pdf

#echo "Linking .lhs"
#pandoc --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable sansfont="DejaVu Sans" --pdf-engine=xelatex ${INPUTS} --to rst+literate_haskell -o ${NAME}.lhs
#mln -s -p -g '*.md' '#1.lhs' >/dev/null || true

echo "Making .html"
stack exec -- pandoc --filter=pandoc-hide-codeblocks --mathml --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable sansfont=Arial --pdf-engine=xelatex ${INPUTS} -o ${NAME}.html --highlight-style=espresso

echo "Building .lhs to check source code is valid"
stack build

echo "Checking for missing files in toc.list:"
for i in *.lhs *.mmd; do grep $i toc.list>/dev/null || echo "Missing file in TOC: $i"; done
for i in ../test/*.lhs; do grep $i toc.list>/dev/null || echo "Missing file in TOC: $i"; done

