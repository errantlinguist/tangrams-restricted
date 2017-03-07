#!/bin/sh

for file in ..\src\main\resources\se\kth\speech\coin\tangrams\content\images\icons\*.svg
do
     /usr/bin/inkscape -z -f "${file}" -w 640 -e "${file}.png"
done