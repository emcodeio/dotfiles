#!/usr/bin/env sh

echo "Bit Depth... (16, 24, 32)?";
read bitdepth;
echo "Sample Rate... (44100, 48000, 96000, 19200)?";
read samplerate
quality="LEI$bitdepth@$samplerate"
echo "Using $quality quality"
for i in *.m4a; do
    echo "Converting $i...";
    afconvert -f WAVE -d LEI${bitdepth}@${samplerate} "$i" -o "${i%.*}.wav";
    echo "...done";
    echo;
done
