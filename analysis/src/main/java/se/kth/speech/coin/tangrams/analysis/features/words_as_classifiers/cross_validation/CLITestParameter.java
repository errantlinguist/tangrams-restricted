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

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Set;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.Option;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import edu.stanford.nlp.util.Sets;
import se.kth.speech.coin.tangrams.analysis.tokenization.Cleaning;
import se.kth.speech.coin.tangrams.analysis.tokenization.TokenFiltering;
import se.kth.speech.coin.tangrams.analysis.tokenization.TokenType;
import se.kth.speech.coin.tangrams.analysis.tokenization.Tokenization;

/**
 * @author <a href="mailto:tcshore@kth.se">Todd Shore</a>
 * @since 5 Jun 2017
 *
 */
enum CLITestParameter implements Supplier<Option> {
	CLEANING("c") {
		@Override
		public Option get() {
			final Cleaning[] possibleVals = Cleaning.values();
			return Option.builder(optName).longOpt("cleaning")
					.desc("A list of cleaning method(s) to use. Possible values: " + Arrays.toString(possibleVals))
					.hasArg().argName("name").build();
		}
	},
	HELP("?") {
		@Override
		public Option get() {
			return Option.builder(optName).longOpt("help").desc("Prints this message.").build();
		}
	},
	ITER_COUNT("i") {
		@Override
		public Option get() {
			return Option.builder(optName).longOpt("iter-count")
					.desc("The number of training/testing iterations to run for each cross-validation dataset.")
					.hasArg().argName("count").type(Number.class).build();
		}
	},
	OUTPATH("o") {
		@Override
		public Option get() {
			return Option.builder(optName).longOpt("outpath").desc("The path to write the data to.").hasArg()
					.argName("path").type(File.class).required().build();
		}
	},
	TEST_CLEANING_POWERSET("cp") {
		@Override
		public Option get() {
			return Option.builder(optName).longOpt("cleaning-powerset").desc(
					"If this flag is present, the powerset of the supplied cleaning methods is tested rather than the set itself.")
					.build();
		}
	},
	TOKEN_FILTERS("tf") {
		@Override
		public Option get() {
			final TokenFiltering[] possibleVals = TokenFiltering.values();
			return Option.builder(optName).longOpt("token-filters").desc(
					"A list of token filtering method(s) to use. Possible values: " + Arrays.toString(possibleVals))
					.hasArg().argName("name").build();
		}
	},
	TOKEN_TYPES("tt") {
		@Override
		public Option get() {
			final TokenType[] possibleVals = TokenType.values();
			return Option.builder(optName).longOpt("token-types")
					.desc("A list of token type(s) to use. Possible values: " + Arrays.toString(possibleVals)).hasArg()
					.argName("name").build();
		}
	},
	TOKENIZERS("tok") {
		@Override
		public Option get() {
			final Tokenization[] possibleVals = Tokenization.values();
			return Option.builder(optName).longOpt("tokenizers")
					.desc("A list of tokenization method(s) to use. Possible values: " + Arrays.toString(possibleVals))
					.hasArg().argName("name").build();
		}
	},
	TRAINING("tr") {
		@Override
		public Option get() {
			final Training[] possibleVals = Training.values();
			return Option.builder(optName).longOpt("training")
					.desc("A list of training method(s) to use. Possible values: " + Arrays.toString(possibleVals))
					.hasArg().argName("name").build();
		}
	};

	private static final Logger LOGGER = LoggerFactory.getLogger(CLITestParameter.class);

	private static final Pattern MULTI_OPT_VALUE_DELIMITER = Pattern.compile("\\s+");

	private static Set<Cleaning> parseCleaningMethods(final CommandLine cl) {
		final String[] names = parseOptEnumValueNames(cl, CLITestParameter.CLEANING.optName);
		final Stream<Cleaning> insts = names == null ? Stream.empty()
				: Arrays.stream(names).map(String::trim).filter(str -> !str.isEmpty()).map(Cleaning::valueOf);
		final EnumSet<Cleaning> result = EnumSet.noneOf(Cleaning.class);
		insts.forEach(result::add);
		return result;
	}

	private static String[] parseOptEnumValueNames(final CommandLine cl, final String optName) {
		final String val = cl.getOptionValue(optName);
		return val == null ? null : MULTI_OPT_VALUE_DELIMITER.split(val);
	}

	static Set<Set<Cleaning>> parseCleaningMethodSets(final CommandLine cl) {
		final Set<Cleaning> cleaningMethods = parseCleaningMethods(cl);
		LOGGER.info("Cleaning methods: {}", cleaningMethods);
		return cl.hasOption(CLITestParameter.TEST_CLEANING_POWERSET.optName) ? Sets.powerSet(cleaningMethods)
				: Collections.singleton(cleaningMethods);
	}

	static Set<TokenFiltering> parseTokenFilteringMethods(final CommandLine cl) {
		final String[] names = parseOptEnumValueNames(cl, CLITestParameter.TOKEN_FILTERS.optName);
		final Stream<TokenFiltering> insts = names == null ? Stream.empty()
				: Arrays.stream(names).map(String::trim).filter(str -> !str.isEmpty()).map(TokenFiltering::valueOf);
		final EnumSet<TokenFiltering> result = EnumSet.noneOf(TokenFiltering.class);
		insts.forEach(result::add);
		return result;
	}

	static Set<Tokenization> parseTokenizationMethods(final CommandLine cl) {
		final String[] names = parseOptEnumValueNames(cl, CLITestParameter.TOKENIZERS.optName);
		final Stream<Tokenization> insts = names == null ? Stream.empty()
				: Arrays.stream(names).map(String::trim).filter(str -> !str.isEmpty()).map(Tokenization::valueOf);
		final EnumSet<Tokenization> result = EnumSet.noneOf(Tokenization.class);
		insts.forEach(result::add);
		return result;
	}

	static Set<TokenType> parseTokenTypes(final CommandLine cl) {
		final String[] names = parseOptEnumValueNames(cl, CLITestParameter.TOKEN_TYPES.optName);
		final Stream<TokenType> insts = names == null ? Stream.empty()
				: Arrays.stream(names).map(String::trim).filter(str -> !str.isEmpty()).map(TokenType::valueOf);
		final EnumSet<TokenType> result = EnumSet.noneOf(TokenType.class);
		insts.forEach(result::add);
		return result;
	}

	static Set<Training> parseTrainingMethods(final CommandLine cl) {
		final String[] names = parseOptEnumValueNames(cl, CLITestParameter.TRAINING.optName);
		final Stream<Training> insts = names == null ? Stream.empty()
				: Arrays.stream(names).map(String::trim).filter(str -> !str.isEmpty()).map(Training::valueOf);
		final EnumSet<Training> result = EnumSet.noneOf(Training.class);
		insts.forEach(result::add);
		return result;
	}

	protected final String optName;

	private CLITestParameter(final String optName) {
		this.optName = optName;
	}
}
