from typing import Sequence

from nltk import ngrams


class CachingNgramFactory(object):
	def __init__(self, max_ngram_length : int):
		self.max_ngram_length_stop = max_ngram_length + 1
		self.cache = {}

	def __call__(self, tokens: Sequence[str]):
		try:
			result = self.cache[tokens]
		except KeyError:
			result = self.__create_ngrams(tokens)
			self.cache[tokens] = result

		return result

	def __create_ngrams(self, tokens: Sequence[str]):
		ngram_lens = range(1, min(self.max_ngram_length_stop, len(tokens)))
		return tuple(len_ngram for ngram_len in ngram_lens for len_ngram in ngrams(tokens, ngram_len))
