#!/bin/bash

NAME=Latency
FORMAT=markdown+tex_math_dollars+yaml_metadata_block

echo "Linking diagrams"
ln -s ../doc/completion-rate.png .
ln -s ../doc/latency-distribution.png .

echo "Making hide-codeblocks filter"
#ghc hide-codeblocks.hs -o hide-codeblocks
stack install # for `hide-codeblocks`

echo "Making TEX"
pandoc --filter=hide-codeblocks --natbib --from=${FORMAT} --standalone --variable mainfont="DejaVu Serif" --variable sansfont=Arial --to=latex --pdf-engine=xelatex ${NAME}.md -o ${NAME}.tex
#--filter=pandoc-citeproc --biblio=Latency.bib

echo "Making PDF"
pandoc --filter=hide-codeblocks --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable sansfont=Arial --pdf-engine=xelatex ${NAME}.md -o ${NAME}.pdf

echo "Linking .lhs"
#pandoc --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable sansfont=Arial --pdf-engine=xelatex ${NAME}.md --to rst+literate_haskell -o ${NAME}.lhs
ln -s ${NAME}.md ${NAME}.lhs >/dev/null || true

echo "Interpreting .lhs"
ghc -pgmL markdown-unlit ${LATENCY}.lhs

echo "Making .html"
pandoc --filter=hide-codeblocks --mathml --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable sansfont=Arial --pdf-engine=xelatex ${NAME}.md -o ${NAME}.html --highlight-style=espresso
