#!/usr/bin/env sh

for i in *.wav; do
    echo "Converting $i...";
    afconvert -d alac -f m4af "$i" -o "${i%.*}.alac.m4a";
    echo "...done";
    echo;
done
