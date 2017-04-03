#!/usr/bin/env python
'''
Created on Apr 3, 2017

@author: tshore
'''

from xml.etree import ElementTree
from collections import defaultdict
#tree = ET.parse('country_data.xml')
#root = tree.getroot()

DEFAULT_NAMESPACE = "http://www.speech.kth.se/higgins/2005/annotation/"
DEFAULT_TAG_PREFIX = "{" + DEFAULT_NAMESPACE + "}"

def create_namespace_tag_name(tag_name):
	return DEFAULT_TAG_PREFIX + tag_name

class AnnotationData(object):
	
	def __init__(self):
		self.tracks = {}
		self.segments = Segments()
		
	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

class AnnotationParser(object):
	
	def __init__(self):
		self.__tag_parsers = {create_namespace_tag_name("tracks") : self.__parse_tracks, create_namespace_tag_name("segments") : self.__parse_segments}
		self.__result = None
	
	def __call__(self, infile):
		self.__result = AnnotationData()
		doc_root = ElementTree.parse(infile)
		tag_name = create_namespace_tag_name("annotation")
		for child in doc_root.iter(tag_name):
			self.__parse_annotation(child)		
			
		return self.__result
			
	def __parse_annotation(self, annot):
		for child in annot:
			tag_name = child.tag
			parser = self.__tag_parsers[tag_name]
			parser(child)
			
	def __parse_tracks(self, tracks):
		source_tag_name = create_namespace_tag_name("source")
		track_data = self.__result.tracks
		for track in tracks:
			track_sources = TrackSources()
			track_data[track.attrib["id"]] = track_sources
			
			for source in track.iter(source_tag_name):
				attrs = source.attrib
				track_sources.sources_by_id[attrs["id"]] = source
				track_sources.sources_by_channel[attrs["channel"]] = source
				track_sources.sources_by_href[attrs["href"]] = source
				
	
	def __parse_segments(self, segments):
		segment_data = self.__result.segments
		for segment in segments:
			attrs = segment.attrib
			segment_data.segments_by_id[attrs["id"]] = segment
			segment_data.track_segments[attrs["track"]].append(segment)
			segment_data.source_segments[attrs["source"]].append(segment)

class Segments(object):
	
	def __init__(self):
		self.segments_by_id = {}
		self.track_segments = defaultdict(list)
		self.source_segments = defaultdict(list)
		
	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)
	
class TrackSources(object):
	def __init__(self):
		self.sources_by_id = {}
		self.sources_by_channel = {}
		self.sources_by_href = {}

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)
		

def merge_annots(annot_data):
		for annot_datum in annot_data:
			
			print(annot_datum)

if __name__ == '__main__':
	import sys
	if len(sys.argv) < 2:
		print("Usage: %s INPUT_PATHS... > OUTFILE" % sys.argv[0], file=sys.stderr)
		exit(64);
	else:
		inpaths = sys.argv[1:]
		parser = AnnotationParser()
		
		infile_data = []
		for inpath in inpaths:
			print("Reading \"%s\"." % inpath, file=sys.stderr)
			with open(inpath, 'r') as inf:
				infile_datum = parser(inf)
				infile_data.append(infile_datum)
				
		merge_annots(infile_data)
		