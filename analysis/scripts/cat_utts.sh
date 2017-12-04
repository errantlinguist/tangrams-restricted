#!/bin/sh

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
