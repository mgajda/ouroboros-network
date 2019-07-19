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
pandoc --filter=pandoc-citeproc --filter=hide-codeblocks --natbib --from=${FORMAT} --standalone --to=latex --pdf-engine=xelatex ${INPUTS} -o ${NAME}.tex --bibliography=Latency.bib
#--filter=pandoc-citeproc --biblio=Latency.bib

echo "Making PDF"
pandoc --filter=hide-codeblocks --filter=pandoc-citeproc          --from=${FORMAT} --pdf-engine=xelatex ${INPUTS} -o ${NAME}.pdf

echo "Linking .lhs"
#pandoc --from=${FORMAT} --pdf-engine=xelatex ${INPUTS} --to rst+literate_haskell -o ${NAME}.lhs
mln -s -p -g '*.md' '#1.lhs' >/dev/null || true

echo "Interpreting .lhs"
ghc -pgmL markdown-unlit ${LATENCY}.lhs

echo "Making .html"
pandoc --filter=hide-codeblocks --mathml --from=${FORMAT} --pdf-engine=xelatex ${INPUTS} -o ${NAME}.html --highlight-style=espresso

echo "Checking for missing files in toc.list:"
for i in *.md *.mmd; do grep $i toc.list>/dev/null || echo "Missing file in TOC: $i"; done
for i in ../test/*.md ../test/*.mmd; do grep $i toc.list>/dev/null || echo "Missing file in TOC: $i"; done

