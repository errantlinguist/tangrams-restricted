#!/bin/sh

#old_pwd=`pwd`
cd "../src/main/resources/se/kth/speech/coin/tangrams/content/images/icons"
for infile in *.svg
do
    #echo -n "${infile} > " >&2
    filename_base=${infile%.*}
    outfile=${filename_base}.png
    inkscape -z -f "${infile}" -w 300 -e "${outfile}"
    #echo ${outfile} >&2 
done
