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

# A script for running all sigificance tests for coreference chains.
# Author: Todd Shore <errantlinguist+github@gmail.com>
# Since: 2018-01-07

exit_code=1

if [ $# -ne 2 ]
then
	echo "Usage: $0 INDIR OUTDIR"
	exit_code=64
else
	indir="$1"
	outdir="$2"
	derived_data_dir="${1}/Derived"
	
	echo "Writing referring language."
	find "${derived_data_dir}" -iname "*utt-referring-tokens-basic-inflected.tsv" -exec ./write_utterance_referring_tokens.py {} + > "${outdir}/utt-referring-tokens-basic-inflected.tsv"
	find "${derived_data_dir}" -iname "*utt-referring-tokens-basic-lemma.tsv" -exec ./write_utterance_referring_tokens.py {} + > "${outdir}/utt-referring-tokens-basic-lemma.tsv"
	ready_data_dir="${1}/Ready"
	
	target_ref_utt_file="${outdir}/target_ref_utts_speaker_instructoronly.tsv"
	echo "Writing instructor target referring language to \"${target_ref_utt_file}\"."
	./write_target_ref_utts.py "${ready_data_dir}" > "${target_ref_utt_file}" 2> "${outdir}/target_ref_utts_speaker_instructoronly.err.txt"
	
	echo "Testing referent within-speaker overlap."
	./coreference_chain_overlap.py -r -w "${target_ref_utt_file}" > "${outdir}/referent-within-speaker-overlap.tsv"
	ref_within_dumpfile=`mktemp --tmpdir "referent-within-speaker-overlap-dump.tsv.XXXXXXXXXXXX"`
	./coreference_chain_overlap.py -r -w -d "${target_ref_utt_file}" > "${ref_within_dumpfile}"
	./coreference_overlap_significance.R "${ref_within_dumpfile}" > "${outdir}/referent-within-speaker-overlap-dump-test.txt"
	
	echo "Testing referent between-speaker overlap."
	./coreference_chain_overlap.py -r -b "${target_ref_utt_file}" > "${outdir}/referent-between-speaker-overlap.tsv"
	ref_between_dumpfile=`mktemp --tmpdir "referent-between-speaker-overlap-dump.tsv.XXXXXXXXXXXX"`
	./coreference_chain_overlap.py -r -b -d "${target_ref_utt_file}" > "${ref_between_dumpfile}"
	./coreference_overlap_significance.R "${ref_between_dumpfile}" > "${outdir}/referent-between-speaker-overlap-dump-test.txt"
	
	echo "Testing referent general convergence overlap."
	./general_convergence_overlap.py -r "${target_ref_utt_file}" > "${outdir}/referent-general-convergence-overlap.tsv"
	./general_convergence_overlap_means.R "${outdir}/referent-general-convergence-overlap.tsv" > "${outdir}/referent-general-convergence-overlap-test.txt"
	
	
	echo "Testing shape within-speaker overlap."
	./coreference_chain_overlap.py -s -w "${target_ref_utt_file}" > "${outdir}/shape-within-speaker-overlap.tsv"
	shape_within_dumpfile=`mktemp --tmpdir "shape-within-speaker-overlap-dump.tsv.XXXXXXXXXXXX"`
	./coreference_chain_overlap.py -s -w -d "${target_ref_utt_file}" > "${shape_within_dumpfile}"
	./coreference_overlap_significance.R "${shape_within_dumpfile}" > "${outdir}/shape-within-speaker-overlap-dump-test.txt"
	
	echo "Testing shape between-speaker overlap."
	./coreference_chain_overlap.py -s -b "${target_ref_utt_file}" > "${outdir}/shape-between-speaker-overlap.tsv"
	shape_between_dumpfile=`mktemp --tmpdir "shape-between-speaker-overlap-dump.tsv.XXXXXXXXXXXX"`
	./coreference_chain_overlap.py -s -b -d "${target_ref_utt_file}" > "${shape_between_dumpfile}"
	./coreference_overlap_significance.R "${shape_between_dumpfile}" > "${outdir}/shape-between-speaker-overlap-dump-test.txt"
	
	echo "Testing shape general convergence overlap."
	./general_convergence_overlap.py -s "${target_ref_utt_file}" > "${outdir}/shape-general-convergence-overlap.tsv"
	./general_convergence_overlap_means.R "${outdir}/shape-general-convergence-overlap.tsv" > "${outdir}/shape-general-convergence-overlap-test.txt"
	
fi

exit ${exit_code}
