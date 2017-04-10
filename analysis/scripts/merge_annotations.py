#!/usr/bin/env python3
'''
Created on Apr 3, 2017

@author: tshore
'''

from lxml import etree
from lxml.builder import ElementMaker
#from xml.etree import ElementTree
from collections import defaultdict

DEFAULT_NAMESPACE = "http://www.speech.kth.se/higgins/2005/annotation/"
# http://stackoverflow.com/a/18340978/1391325
etree.register_namespace("hat", DEFAULT_NAMESPACE)
DEFAULT_TAG_PREFIX = "{" + DEFAULT_NAMESPACE + "}"

class AnnotationData(object):
	
	def __init__(self, qname_factory):
		self.qname_factory = qname_factory
		self.tracks = {}
		self.segments = Segments(qname_factory)
		
	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)
	
	def add(self, other):
		for track_id, other_track in other.tracks.items():
			if track_id in self.tracks:
				raise ValueError("Track ID \"%s\" already in dict." % track_id)
			else:
				self.tracks[track_id] = other_track
				
		self.segments.add(other.segments)
		
	def create_xml_element(self):
		# http://stackoverflow.com/a/22902367/1391325
		em = ElementMaker(nsmap={None: DEFAULT_NAMESPACE })
		result = em("annotation")
		tracks_elem = etree.SubElement(result, self.qname_factory("tracks"))
		for track_id, track_data in self.tracks.items():
			track_elem = etree.SubElement(tracks_elem, self.qname_factory("track"), attrib={"id" : track_id})
			sources_elem = etree.SubElement(track_elem, self.qname_factory("sources"))
			for track_source in track_data.sources_by_id.values():
				sources_elem.append(track_source)
		
		segments_elem = self.segments.create_xml_element()
		result.append(segments_elem)
			
		return result

class AnnotationParser(object):
	
	def __init__(self, id_prefix, channel_offset, qname_factory):
		self.id_prefix = id_prefix
		self.channel_offset = channel_offset
		self.qname_factory = qname_factory
		self.__tag_parsers = {self.qname_factory("tracks") : self.__parse_tracks, self.qname_factory("segments") : self.__parse_segments}
		self.__result = None
	
	def __call__(self, infile):
		self.__result = AnnotationData(self.qname_factory)
		doc_root = etree.parse(infile)
		tag_name = self.qname_factory("annotation")
		for child in doc_root.iter(tag_name):
			self.__parse_annotation(child)		
			
		return self.__result
			
	def __parse_annotation(self, annot):
		for child in annot:
			tag_name = child.tag
			parser = self.__tag_parsers[tag_name]
			parser(child)
			
	def __parse_tracks(self, tracks):
		source_tag_name = self.qname_factory("source")
		track_data = self.__result.tracks
		for track in tracks:
			track_sources = TrackSources()
			track_attrs = track.attrib
			track_id = self.id_prefix + track_attrs["id"]
			track_data[track_id] = track_sources
			track_attrs["id"] = track_id 
			
			for source in track.iter(source_tag_name):
				attrs = source.attrib
				source_id = self.id_prefix + attrs["id"]
				track_sources.sources_by_id[source_id] = source
				attrs["id"] = source_id
				channel = int(attrs["channel"]) + self.channel_offset
				track_sources.sources_by_channel[channel] = source
				attrs["channel"] = str(channel)
				track_sources.sources_by_href[attrs["href"]] = source
				
	
	def __parse_segments(self, segments):
		segment_data = self.__result.segments
		for segment in segments:
			attrs = segment.attrib
			segment_id = self.id_prefix + attrs["id"]
			attrs["id"] = segment_id
			segment_data.segments_by_id[segment_id] = segment
			track_id = self.id_prefix + attrs["track"]
			segment_data.track_segments[track_id].append(segment)
			attrs["track"] = track_id
			source_id = self.id_prefix + attrs["source"]
			segment_data.source_segments[source_id].append(segment)
			attrs["source"] = source_id

class Segments(object):
	
	def __init__(self, qname_factory):
		self.qname_factory = qname_factory
		self.segments_by_id = {}
		self.track_segments = defaultdict(list)
		self.source_segments = defaultdict(list)
		
	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)
	
	def add(self, other):
		for segment_id, segment in other.segments_by_id.items():
			if segment_id in self.segments_by_id:
				raise ValueError("Segment ID \"%s\" already in dict." % segment_id)
			else:
				self.segments_by_id[segment_id] = segment
				self.track_segments.update(other.track_segments)
				self.source_segments.update(other.source_segments)
				
	def create_xml_element(self):
		result = etree.Element(self.qname_factory("segments"))
		for segment in self.segments_by_id.values():
			result.append(segment)
			
		return result
	
class TrackSources(object):
	def __init__(self):
		self.sources_by_id = {}
		self.sources_by_channel = {}
		self.sources_by_href = {}

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)
		
def create_namespace_tag_name(tag_name):
	return DEFAULT_TAG_PREFIX + tag_name

if __name__ == '__main__':
	import sys
	if len(sys.argv) < 2:
		print("Usage: %s INPUT_PATHS... > OUTFILE" % sys.argv[0], file=sys.stderr)
		sys.exit(64);
	else:
		import os.path
		import tempfile

		inpaths = sys.argv[1:]
		annot_data = []
		qname_factory = create_namespace_tag_name
		for channel_offset, inpath in enumerate(inpaths):
			print("Reading \"%s\"." % inpath, file=sys.stderr)
			id_prefix = os.path.splitext(os.path.basename(inpath))[0] + "-"
			parser = AnnotationParser(id_prefix, channel_offset, qname_factory)
			with open(inpath, 'r') as inf:
				infile_datum = parser(inf)
				annot_data.append(infile_datum)
				
		result = annot_data[0]
		if len(annot_data) > 1:
			next_data = annot_data[1:]
			for next_datum in next_data:
				result.add(next_datum)
				
		annot_elem = result.create_xml_element()
		annot_tree = etree.ElementTree(annot_elem)
	
		tmpfile = tempfile.mkstemp(text=True)
		tmpfile_path = tmpfile[1]
		try:
			encoding = "utf-8"
			annot_tree.write(tmpfile_path, encoding=encoding, xml_declaration=True, pretty_print=True)
			with open(tmpfile_path, 'r', encoding=encoding) as inf:
				print(inf.read())
		finally:
			os.close(tmpfile[0])
			os.remove(tmpfile_path)
				
		