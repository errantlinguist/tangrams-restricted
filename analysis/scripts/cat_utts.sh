#!/bin/sh

# This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
#
# tangrams is free software: you can redistribute it and/or modify
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

# A script for concatenating all tabular utterance files (named "utts.xml").
# Author: Todd Shore <errantlinguist+github@gmail.com>
# Since: 2017-12-04

exit_code=1

if [ $# -lt 1 ]
then
	echo "Usage: $0 INPATHS..."
	exit_code=64
else
	inpaths="$@"
	header=`find "${inpaths}" -type f -name "utts.tsv" -exec head -n1 {} \; | uniq`
	header_linecount=`echo "${header}" | wc -l`
	if [ ${header_linecount} -gt 1 ]
	then
		echo "More than one unique header found! Aborting." 1>&2
	else
		echo "${header}"
		find "${inpaths}" -type f -name "utts.tsv" -exec tail -n+2 {} \;
	fi
	

fi

exit ${exit_code}
