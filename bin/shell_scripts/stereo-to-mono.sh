#!/usr/bin/env sh

for i in *.wav; do
    echo "Downmixing $i...";
    afconvert -v -d LEI32 -c 1 --mix ${i} ${i%.*}.mono.wav;
    echo "...done";
    echo;
done
