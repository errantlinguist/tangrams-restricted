#!/bin/sh

if [ $# -lt 1 ]
then
	echo "Usage: $0 INPATHS..."
	exit_code=64
else
	find "$@" -type f -name "utts.tsv" -exec tail -n+2 {} \;
fi

exit ${exit_code}
