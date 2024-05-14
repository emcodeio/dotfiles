#!/usr/bin/env sh

for i in *.m4a; do
    echo "Converting $i...";
    afconvert -d aac -f m4af -b 256000 -q 127 -s 2 "$i" -o "${i%.*}.aac.m4a";
    echo "...done";
    echo;
done
