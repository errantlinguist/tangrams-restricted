#!/bin/sh

# This file is part of Tangrams-restricted.
#
# Tangrams-restricted is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# A script for converting all SVG icon files into PNG using Inkscape.
#
# Author: Todd Shore <errantlinguist+github@gmail.com>
# Since: 2017-03-07

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
