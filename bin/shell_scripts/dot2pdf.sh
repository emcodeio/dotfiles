#!/usr/bin/env zsh
for GFILE in *.dot;
do
    GNAME="${GFILE%.*}";
    dot -Tpdf "$GFILE" -o "$GNAME.pdf";
done
