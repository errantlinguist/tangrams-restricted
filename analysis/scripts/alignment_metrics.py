from decimal import Decimal
from typing import FrozenSet


def token_type_overlap_ratio(token_types: FrozenSet[str], preceding_token_types: FrozenSet[str]) -> Decimal:
	unified_token_type_count = len(token_types.union(
		preceding_token_types))
	overlapping_token_type_count = len(token_types.intersection(
		preceding_token_types))
	return Decimal(overlapping_token_type_count) / Decimal(unified_token_type_count)
