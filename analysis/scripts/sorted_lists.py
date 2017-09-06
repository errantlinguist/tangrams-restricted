import bisect
import itertools


class SortedList(list):
	"""
	see <https://docs.python.org/2/library/bisect.html#searching-sorted-lists>
	"""

	def __init__(self, *args) -> None:
		super().__init__(*args)

	@staticmethod
	def __index_lt(sublist, elem):
		"""Find the index of the first element less than elem"""
		result = bisect.bisect_left(sublist, elem)
		if result:
			return result
		else:
			raise ValueError

	@staticmethod
	def __slice_lt(sublist, elem):
		"""Find values less than elem"""
		idx = SortedList.__index_lt(sublist, elem)
		return sublist[0:idx - 1]

	def index(self, elem, start: int = 0, stop: int = ...) -> int:
		result = bisect.bisect_left(self, elem, start, stop)
		end_idx = min(stop, len(self))
		if result < end_idx and self[result] == elem:
			return result
		else:
			raise ValueError

	def _index_ge(self, x):
		"""Find the index of the first value greater than or equal to x"""
		result = bisect.bisect_left(self, x)
		if result >= len(self):
			raise ValueError
		else:
			return result

	def iter_between(self, start, end):
		# Find the index of the first element equal to or greater than the start element
		start_idx = self._index_ge(start)
		end_idx = SortedList.__index_lt(self, end)
		return itertools.islice(self, start_idx, end_idx)

	def slice_between(self, start, end):
		elems_after_start = self.slice_ge(start)
		return SortedList.__slice_lt(elems_after_start, end)

	def slice_le(self, x):
		"""Find values less than or equal to x"""
		idx = bisect.bisect_right(self, x)
		if idx:
			return self[0: idx - 1]
		else:
			raise ValueError

	def slice_lt(self, x):
		"""Find values less than x"""
		return SortedList.__slice_lt(self, x)

	def slice_ge(self, x):
		"""Find values greater than or equal to x"""
		idx = self._index_ge(x)
		return self[idx:]

	def slice_gt(self, x):
		"""Find values greater than x"""
		idx = bisect.bisect_right(self, x)
		length = len(self)
		if idx == length:
			raise ValueError
		else:
			return self[idx: length]
