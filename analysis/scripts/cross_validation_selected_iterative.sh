#!/bin/sh

TARGET_DIR="target"
CLASSPATH_JARFILE="${TARGET_DIR}/analysis-0.0.1-SNAPSHOT-jar-with-dependencies.jar"
HEAP_SIZE="5120m"
# HEAP_SIZE="10240m"
echo "Will use heap space size of ${HEAP_SIZE}."
MAIN_CLASS="se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.CombiningBatchJobTestSingleFileWriter"

exit_status=1
usage_msg="Usage: $0 INPATH OUTDIR"

if [ -f "${CLASSPATH_JARFILE}" ]
then
	inpath="${1?"${usage_msg}"}"
	outdir="${2?"${usage_msg}"}"

	# https://stackoverflow.com/a/3466183/1391325
	case `uname -s` in
		*CYGWIN*) echo "Converting paths to Windows format."; inpath="`cygpath -w "${inpath}"`"; outdir="`cygpath -w "${outdir}"`" ;;
	esac

	echo "Will read files from \"${inpath}\"."
	echo "Output directory is \"${outdir}\"."

	java -server -Xmx${HEAP_SIZE} -cp ${CLASSPATH_JARFILE} ${MAIN_CLASS} "${inpath}" -o "${outdir}/trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.tsv" -c "METALANGUAGE FILLERS DISFLUENCIES DUPLICATES" -tok "STANFORD_NPS_WITHOUT_PPS" -tt "INFLECTED" -tf "STOPWORDS" -tr "ONE_NEG_ITERATIVE ALL_NEG_ITERATIVE DIALOGIC_ITERATIVE" > "${outdir}/trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.out" 2> "${outdir}/trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.err" &&
	java -server -Xmx${HEAP_SIZE} -cp ${CLASSPATH_JARFILE} ${MAIN_CLASS} "${inpath}" -o "${outdir}/trainingtest-cleaning-NPsWithoutPPs.tsv" -c "METALANGUAGE FILLERS DISFLUENCIES DUPLICATES" -tok "STANFORD_NPS_WITHOUT_PPS" -tt "INFLECTED" -tf "NO_FILTER" -tr "ONE_NEG_ITERATIVE ALL_NEG_ITERATIVE DIALOGIC_ITERATIVE" > "${outdir}/trainingtest-cleaning-NPsWithoutPPs.out" 2> "${outdir}/trainingtest-cleaning-NPsWithoutPPs.err" &&
	java -server -Xmx${HEAP_SIZE} -cp ${CLASSPATH_JARFILE} ${MAIN_CLASS} "${inpath}" -o "${outdir}/trainingtest-basic.tsv" -c "" -tok "STANFORD_BASIC" -tt "INFLECTED" -tf "NO_FILTER" -tr "ONE_NEG_ITERATIVE ALL_NEG_ITERATIVE DIALOGIC_ITERATIVE" > "${outdir}/trainingtest-basic.out" 2> "${outdir}/trainingtest-basic.err" &&
	java -server -Xmx${HEAP_SIZE} -cp ${CLASSPATH_JARFILE} ${MAIN_CLASS} "${inpath}" -o "${outdir}/trainingtest-basic-stopwordfillers.tsv" -c "FILLERS" -tok "STANFORD_BASIC" -tt "INFLECTED" -tf "STOPWORDS" -tr "ONE_NEG_ITERATIVE ALL_NEG_ITERATIVE DIALOGIC_ITERATIVE" > "${outdir}/trainingtest-basic-stopwordfillers.out" 2> "${outdir}/trainingtest-basic-stopwordfillers.err"
	exit_status=$?
else
	echo "Must be in the project base directory, i.e. the parent of directory \"${TARGET_DIR}\"."
	echo "${usage_msg}"
	exit_status=64
fi

exit ${exit_status}