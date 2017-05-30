/*
 *  This file is part of se.kth.speech.coin.tangrams-restricted.analysis.
 *
 *  tangrams is free software: you can redistribute it and/or modify
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

import java.time.LocalDateTime;

public final class BatchJobSummary {

	private final TestParameters testParams;

	private final Tester.Result testResults;

	private final LocalDateTime testTimestamp;

	BatchJobSummary(final LocalDateTime testTimestamp, final TestParameters testParams,
			final Tester.Result testResults) {
		this.testTimestamp = testTimestamp;
		this.testParams = testParams;
		this.testResults = testResults;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("BatchJobSummary [testParams=");
		builder.append(testParams);
		builder.append(", testResults=");
		builder.append(testResults);
		builder.append(", testTimestamp=");
		builder.append(testTimestamp);
		builder.append("]");
		return builder.toString();
	}

	/**
	 * @return the testParams
	 */
	TestParameters getTestParams() {
		return testParams;
	}

	/**
	 * @return the testResults
	 */
	Tester.Result getTestResults() {
		return testResults;
	}

	/**
	 * @return the testTimestamp
	 */
	LocalDateTime getTestTimestamp() {
		return testTimestamp;
	}
}