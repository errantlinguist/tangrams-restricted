@echo off

set "CLASSPATH=target\analysis-0.0.1-SNAPSHOT-jar-with-dependencies.jar"
set "HEAP_SIZE=5120m"
set "MAIN_CLASS=se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.CombiningBatchJobTestSingleFileWriter"

rem https://stackoverflow.com/a/1292094/1391325
set /A arg_count=0
for %%x in (%*) do set /A arg_count+=1

set /A exit_code=1

IF /I "%arg_count%" EQU "2" (
	set "inpath=%~1"
	echo Will read files from "%inpath%".
	set "outdir=%~2"
	echo Output directory is "%outdir%".
	java -server -Xmx%HEAP_SIZE% -cp %CLASSPATH% %MAIN_CLASS% "%inpath%" -o "%outdir%\trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.tsv" -c "FILLERS DISFLUENCIES DUPLICATES" -tok "NPS_WITHOUT_PPS" -tt "INFLECTED" -tf "STOPWORDS" -tr "ONE_NEG ALL_NEG DIALOGIC" > %outdir%\trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.out 2> %outdir%\trainingtest-cleaning-NPsWithoutPPs-stopwordfilter.err
	java -server -Xmx%HEAP_SIZE% -cp %CLASSPATH% %MAIN_CLASS% "%inpath%" -o "%outdir%\trainingtest-cleaning-NPsWithoutPPs.tsv" -c "FILLERS DISFLUENCIES DUPLICATES" -tok "NPS_WITHOUT_PPS" -tt "INFLECTED" -tf "NO_FILTER" -tr "ONE_NEG ALL_NEG DIALOGIC" > "%outdir%\trainingtest-cleaning-NPsWithoutPPs.out" 2> "%outdir%\trainingtest-cleaning-NPsWithoutPPs.err"
	java -server -Xmx%HEAP_SIZE% -cp %CLASSPATH% %MAIN_CLASS% "%inpath%" -o "%outdir%\trainingtest-basic.tsv" -c "" -tok "BASIC" -tt "INFLECTED" -tf "NO_FILTER" -tr "ONE_NEG ALL_NEG DIALOGIC" > "%outdir%\trainingtest-basic.out" 2> "%outdir%\trainingtest-basic.err"
	java -server -Xmx%HEAP_SIZE% -cp %CLASSPATH% %MAIN_CLASS% "%inpath%" -o "%outdir%\trainingtest-basic-stopwordfillers.tsv" -c "FILLERS" -tok "BASIC" -tt "INFLECTED" -tf "STOPWORDS" -tr "ONE_NEG ALL_NEG DIALOGIC" > "%outdir%\trainingtest-basic-stopwordfillers.out" 2> "%outdir%\trainingtest-basic-stopwordfillers.err"
	set /A exit_code=%ERRORLEVEL%
) else (
	echo Usage: %0 INPATH OUTDIR
	rem https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx#ERROR_BAD_ARGUMENTS
	set /A exit_code=160
)

exit /B %exit_code%