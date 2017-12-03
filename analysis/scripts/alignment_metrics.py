"""
Metrics used for analyzing alignment of dialogue participant language throughout the course of a dialogue.

See Shore, T. et al. (2018) "Towards a common dataset for research on conceptual pacts and alignment in task-oriented dialogue". Submitted to LREC 2018, Eleventh International Conference on Language Resources and Evaluation.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

from decimal import Decimal
from typing import FrozenSet


def token_type_overlap_ratio(token_types: FrozenSet[str], preceding_token_types: FrozenSet[str]) -> Decimal:
	unified_token_type_count = len(token_types.union(
		preceding_token_types))
	overlapping_token_type_count = len(token_types.intersection(
		preceding_token_types))
	return Decimal(overlapping_token_type_count) / Decimal(unified_token_type_count)
