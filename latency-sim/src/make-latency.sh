#!/bin/bash

NAME=Latency
FORMAT=markdown+tex_math_dollars+yaml_metadata_block

ghc hide-codeblocks.hs -o hide-codeblocks
echo "TEX"
pandoc --filter=./hide-codeblocks --from=${FORMAT} --standalone --variable mainfont="DejaVu Serif" --variable sansfont=Arial --to=latex --pdf-engine=xelatex ${NAME}.md -o ${NAME}.tex
echo "PDF"
pandoc --filter=./hide-codeblocks --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable sansfont=Arial --pdf-engine=xelatex ${NAME}.md -o ${NAME}.pdf
echo ".lhs"
#pandoc --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable sansfont=Arial --pdf-engine=xelatex ${NAME}.md --to rst+literate_haskell -o ${NAME}.lhs
ln -s ${NAME}.md ${NAME}.lhs >/dev/null || true
ghc -pgmL markdown-unlit ${LATENCY}.lhs
echo ".html"
pandoc --filter=./hide-codeblocks --mathml --from=${FORMAT} --variable mainfont="DejaVu Serif" --variable sansfont=Arial --pdf-engine=xelatex ${NAME}.md -o ${NAME}.html --highlight-style=espresso
