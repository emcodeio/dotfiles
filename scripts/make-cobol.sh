#!/bin/bash

name=$1
if [ -z "$name" ]; then
   echo "Usage: $0 name"
   exit 1;
fi

if [ -z "$2" ]; then
   app=$(echo $name | tr [A-Z] [a-z])
   module=$(echo $name | tr [a-z] [A-Z])
else
   app=$1
   module=$2
fi

COB=${app}/source/cobol/${module}.cob

mkdir -p $(dirname $COB)

cat <<EOF > $COB
      *> ${module}
      *>
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ${module}.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-AREA.
           05  CALC1   PIC 9(8).
      *>
       PROCEDURE DIVISION.

           TODO CODE HERE

           STOP RUN.
EOF

echo "Wrote cobol: $COB"

