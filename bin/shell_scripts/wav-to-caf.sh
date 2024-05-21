#!/usr/bin/env sh

for i in *.wav; do
    echo "Converting $i...";
    afconvert -d 0 -f caff --soundcheck-generate "$i" -o "${i%.*}.caf";
    echo "...done";
done
