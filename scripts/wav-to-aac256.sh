#!/usr/bin/env sh

for i in *.wav; do
    echo "Converting $i...";
    afconvert -d 0 -f caff --soundcheck-generate "$i" -o "${i%.*}.caf";
    afconvert -d aac -f m4af -ue pgcm 2 --soundcheck-read -b 256000 -q 127 -s 2 "${i%.*}.caf" -o "${i%.*}.aac.m4a";
    rm *.caf;
    echo "...done";
    echo;
done
