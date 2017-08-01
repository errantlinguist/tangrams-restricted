#!/bin/sh

CLASSPATH="target/analysis-0.0.1-SNAPSHOT-jar-with-dependencies.jar"
HEAP_SIZE="5120m"
MAIN_CLASS="se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.CombiningBatchJobTestSingleFileWriter"
SCRIPTS_DIR_NAME="scripts"

usage_msg="Usage: $0 INPATH OUTDIR"

inpath="${1?"${usage_msg}"}"
outdir="${2?"${usage_msg}"}"

exit_code=1

# https://stackoverflow.com/a/13864829/1391325
if [ -z ${0+x} ]
then
	# No script name to use for deduction; just assume that the shell has the right PWD
	exit_code=0
else
	script_name="${0##*/}"
	# http://mywiki.wooledge.org/BashFAQ/028#Using_PWD
	if [ -f "${SCRIPTS_DIR_NAME}/${script_name}" ]
	then
		# PWD is correct
		exit_code=0
	else
		if [ -f "./${script_name}" ]
		then
			# PWD is the scripts directory; Go one level up
			cd ..
			exit_code=0
		else
			echo "Current working directory must be the base project directory, i.e. parent to the directory \"${SCRIPTS_DIR_NAME}\".
${usage_msg}"
			exit_code=64
		fi
	fi
fi

if [ ${exit_code} -eq 0 ]
then
	# https://stackoverflow.com/a/3466183/1391325
	case `uname -s` in
		*CYGWIN*) echo "Converting paths to Windows format."; inpath="`cygpath -w "${inpath}"`"; outdir="`cygpath -w "${outdir}"`" ;;
	esac

	echo "Will read files from \"${inpath}\"."
	echo "Output directory is \"${outdir}\"."

	java -server -Xmx${HEAP_SIZE} -cp ${CLASSPATH} ${MAIN_CLASS} "${inpath}" -o "${outdir}/trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.tsv" -c "FILLERS DISFLUENCIES DUPLICATES" -tok "NPS_WITHOUT_PPS" -tt "INFLECTED" -tf "STOPWORDS" -tr "ONE_NEG ALL_NEG DIALOGIC" > "${outdir}/trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.out" 2> "${outdir}/trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.err" &&
	exit_code=$?
	java -server -Xmx${HEAP_SIZE} -cp ${CLASSPATH} ${MAIN_CLASS} "${inpath}" -o "${outdir}/trainingtest-cleaning-NPsWithoutPPs.tsv" -c "FILLERS DISFLUENCIES DUPLICATES" -tok "NPS_WITHOUT_PPS" -tt "INFLECTED" -tf "NO_FILTER" -tr "ONE_NEG ALL_NEG DIALOGIC" > "${outdir}/trainingtest-cleaning-NPsWithoutPPs.out" 2> "${outdir}/trainingtest-cleaning-NPsWithoutPPs.err" &&
	exit_code=$?
	java -server -Xmx${HEAP_SIZE} -cp ${CLASSPATH} ${MAIN_CLASS} "${inpath}" -o "${outdir}/trainingtest-basic.tsv" -c "" -tok "BASIC" -tt "INFLECTED" -tf "NO_FILTER" -tr "ONE_NEG ALL_NEG DIALOGIC" > "${outdir}/trainingtest-basic.out" 2> "${outdir}/trainingtest-basic.err" &&
	exit_code=$?
	java -server -Xmx${HEAP_SIZE} -cp ${CLASSPATH} ${MAIN_CLASS} "${inpath}" -o "${outdir}/trainingtest-basic-stopwordfillers.tsv" -c "FILLERS" -tok "BASIC" -tt "INFLECTED" -tf "STOPWORDS" -tr "ONE_NEG ALL_NEG DIALOGIC" > "${outdir}/trainingtest-basic-stopwordfillers.out" 2> "${outdir}/trainingtest-basic-stopwordfillers.err"
	exit_code=$?
fi

exit ${exit_code}