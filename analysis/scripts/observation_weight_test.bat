@echo off
rem https://stackoverflow.com/a/9681923/1391325
setlocal enabledelayedexpansion

set "TARGET_DIR=target"
set "CLASSPATH_JARFILE=%TARGET_DIR%\analysis-0.0.1-SNAPSHOT-jar-with-dependencies.jar"
set "HEAP_SIZE=5120m"
rem set "HEAP_SIZE=10240m"
echo Will use heap space size of %HEAP_SIZE%.
set "MAIN_CLASS=se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.ObservationWeightTestWriter"

rem https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx#ERROR_BAD_ARGUMENTS
set /A EXIT_USAGE=160

set /A exit_status=1
set "usage_msg=Usage: %0 INPATH OUTDIR"

if exist "%CLASSPATH_JARFILE%" (
	rem https://stackoverflow.com/a/1292094/1391325
	set /A arg_count=0
	for %%x in (%*) do set /A arg_count+=1
	if /I "!arg_count!" EQU "2" (
		set "inpath=%~1"
		echo Will read files from "!inpath!".
		set "outdir=%~2"
		echo Output directory is "!outdir!".
		
		java -server -Xmx%HEAP_SIZE% -cp %CLASSPATH_JARFILE% %MAIN_CLASS% "!inpath!" -o "!outdir!\trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.tsv" -c "FILLERS DISFLUENCIES DUPLICATES" -tok "STANFORD_NPS_WITHOUT_PPS" -tt "INFLECTED" -tf "STOPWORDS" -tr "ALL_NEG DIALOGIC" > !outdir!\trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.out 2> !outdir!\trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.err
		set /A exit_status=!ERRORLEVEL!
	) else (
		echo %usage_msg%
		set /A exit_status=%EXIT_USAGE%
	)
) else (
	echo Must be in the project base directory, i.e. the parent of directory "%TARGET_DIR%".
	echo %usage_msg%
	set /A exit_status=%EXIT_USAGE%
)

exit /B !exit_status!