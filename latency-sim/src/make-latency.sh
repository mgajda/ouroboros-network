#!/bin/bash

NAME=Latency
INPUTS=$(cat toc.list)
FORMAT=markdown+tex_math_dollars+yaml_metadata_block+citations

echo "Linking diagrams"
ln -s ../doc/completion-rate.png .
ln -s ../doc/latency-distribution.png .

echo "Making hide-codeblocks filter"
#ghc hide-codeblocks.hs -o hide-codeblocks
stack install # for `hide-codeblocks`

echo "Making TEX"
pandoc --filter=pandoc-citeproc --filter=hide-codeblocks --natbib --from=${FORMAT} --standalone --variable mainfont="DejaVu Serif" --variable sansfont=Arial --to=latex --pdf-engine=xelatex ${INPUTS} -o ${NAME}.tex --csl="chicago-fullnote-bibliography.csl" --bibliography=Latency.bib
#--filter=pandoc-citeproc --biblio=Latency.bib

echo "Making PDF"
pandoc --filter=hide-codeblocks --filter=pandoc-citeproc          --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable sansfont=Arial --pdf-engine=xelatex ${INPUTS} -o ${NAME}.pdf

echo "Linking .lhs"
#pandoc --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable sansfont=Arial --pdf-engine=xelatex ${INPUTS} --to rst+literate_haskell -o ${NAME}.lhs
mln -s -p -g '*.md' '#1.lhs' >/dev/null || true

echo "Interpreting .lhs"
ghc -pgmL markdown-unlit ${LATENCY}.lhs

echo "Making .html"
pandoc --filter=hide-codeblocks --mathml --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable sansfont=Arial --pdf-engine=xelatex ${INPUTS} -o ${NAME}.html --highlight-style=espresso
