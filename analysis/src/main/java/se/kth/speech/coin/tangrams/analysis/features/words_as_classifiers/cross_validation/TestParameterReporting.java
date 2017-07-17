/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  se.kth.speech.coin.tangrams-restricted.analysis is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
package se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation;

import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import it.unimi.dsi.fastutil.objects.Object2IntMap;
import se.kth.speech.coin.tangrams.analysis.features.weka.EntityInstanceAttributeContext;
import se.kth.speech.coin.tangrams.analysis.features.words_as_classifiers.cross_validation.Tester.CrossValidationTestSummary;
import se.kth.speech.coin.tangrams.iristk.EventTimes;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 5 Jun 2017
 *
 */
final class TestParameterReporting {

	/**
	 * <strong>NOTE:</strong> This is for SPSS compatibility, which does not
	 * allow e.g.&nbsp;<code>"-"</code> as part of a variable name.
	 *
	 * @see <a href=
	 *      "https://www.ibm.com/support/knowledgecenter/en/SSLVMB_21.0.0/com.ibm.spss.statistics.help/syn_variables_variable_names.htm">SPSS
	 *      documentation</a>
	 */
	private static final String SUBCOL_NAME_DELIM = ".";

	static final DateTimeFormatter TIMESTAMP_FORMATTER = EventTimes.FORMATTER;

	private static Stream<String> createTrainingDataColHeaders() {
		return EntityInstanceAttributeContext.getClassValues().stream()
				.map(classVal -> "TRAIN_INSTS" + SUBCOL_NAME_DELIM + classVal);
	}

	static Stream<String> createCleaningMethodBooleanValues(final Collection<? super Cleaning> cleaningMethods) {
		final IntStream vals = Arrays.stream(Cleaning.values()).map(cleaningMethods::contains)
				.mapToInt(boolVal -> boolVal ? 1 : 0);
		return vals.mapToObj(Integer::toString);
	}

	static Stream<String> createColHeaders(final List<DialogueAnalysisSummaryFactory.SummaryDatum> summaryDataToWrite) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add("TIME");
		createTestMethodColumnHeaders().forEachOrdered(resultBuilder);
		createTrainingDataColHeaders().forEachOrdered(resultBuilder);
		summaryDataToWrite.stream().map(DialogueAnalysisSummaryFactory.SummaryDatum::toString)
				.forEachOrdered(resultBuilder);
		return resultBuilder.build();
	}

	static Stream<String> createTestMethodColumnHeaders() {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		final String cleaningMethodPrefix = Cleaning.class.getSimpleName() + SUBCOL_NAME_DELIM;
		Arrays.stream(Cleaning.values()).map(method -> cleaningMethodPrefix + method).forEachOrdered(resultBuilder);
		resultBuilder.add(Tokenization.class.getSimpleName().toString());
		resultBuilder.add(TokenType.class.getSimpleName());
		resultBuilder.add(TokenFiltering.class.getSimpleName());
		resultBuilder.add(Training.class.getSimpleName());
		return resultBuilder.build();
	}

	static Stream<String> createTestMethodRowCellValues(final TestParameters testParams,
			final Function<? super Set<Cleaning>, Stream<String>> cleaningMethodReprFactory) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		final Set<Cleaning> cleaningMethods = testParams.getCleaning();
		cleaningMethodReprFactory.apply(cleaningMethods).forEachOrdered(resultBuilder);
		resultBuilder.add(testParams.getTokenization().toString());
		resultBuilder.add(testParams.getTokenType().toString());
		resultBuilder.add(testParams.getTokenFiltering().toString());
		resultBuilder.add(testParams.getTrainingMethod().toString());
		return resultBuilder.build();
	}

	static Stream<String> createTestParamRowCellValues(final BatchJobSummary summary) {
		final Stream.Builder<String> resultBuilder = Stream.builder();
		resultBuilder.add(TIMESTAMP_FORMATTER.format(summary.getTestTimestamp()));
		createTestMethodRowCellValues(summary.getTestParams(),
				TestParameterReporting::createCleaningMethodBooleanValues).forEachOrdered(resultBuilder);
		return resultBuilder.build();
	}

	static Stream<Object> createTrainingDataRowCellValues(final CrossValidationTestSummary cvTestSummary) {
		final Object2IntMap<String> trainingInstCounts = cvTestSummary.getTrainingInstanceCounts();
		return EntityInstanceAttributeContext.getClassValues().stream().map(trainingInstCounts::getInt);
	}

	private TestParameterReporting() {
	}

}
