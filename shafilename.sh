#!/bin/bash
for file in *.csv
do
    if [ -f "$file" ]
    then
        base=${file##*/}
        noext=${base%.*}
        sha=$(printf '%s' "$noext" | openssl sha1 | awk '{print $2}')
        newfile=${noext}.${sha} 
        mv $file $newfile.csv
    fi
done
