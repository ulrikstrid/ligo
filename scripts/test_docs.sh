#!/bin/sh
FILES=$(find ./gitlab-pages -name '*.md')
ERRORS=false

for file in $FILES 
do
    RES=$(_build/default/src/bin/cli.exe run-doc $file)
    if ! $RES
    then
        ERRORS=true
        echo $RES
        echo "\033[31m ^^^ Something went wrong when trying to compile: $file\033[m";
    fi
done

if $ERRORS 
then
    exit 1;
else 
    exit 0;
fi
