#!/usr/bin/env sh

for i in *.wav; do
    afconvert -d 0 -f caff --soundcheck-generate $i -o ${i%.wav}.caf;
done


for i in *.caf; do
    baseFilename=`basename "${i}" .caf` && afconvert -f WAVE -d LEI24@48000 "${i}" "${baseFilename}.wav"; done



for i in *.wav; do
    afconvert -v -f m4af -d aac -b 256000 -q 127 -s 2 "${i}";
done
