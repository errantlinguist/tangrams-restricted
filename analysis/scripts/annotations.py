from collections import defaultdict
import re
from xml.sax.saxutils import escape

from lxml import etree
from lxml.builder import ElementMaker

HAT_DATA_NAMESPACE = "http://www.speech.kth.se/higgins/2005/annotation/"

__DIGITS_PATTERN = re.compile('(\d+)')
__WHITESPACE_PATTERN = re.compile('\s+')

class AnnotationData(object):
	
	def __init__(self, qname_factory, namespace, encoding):
		self.qname_factory = qname_factory
		self.namespace = namespace
		self.encoding = encoding
		self.tracks = {}
		self.segments = Segments(qname_factory)
		
	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)
	
	def add(self, other):
		old_track_count = len(self.tracks)
		for track_id, other_track in other.tracks.items():
			if track_id in self.tracks:
				raise ValueError("Track ID \"%s\" already in dict." % track_id)
			else:
				self.tracks[track_id] = other_track
		# Remove channel IDs because HAT doesn't support multiple tracks for one Annotation which each have "channel" attrs
		if old_track_count < len(self.tracks):
			for track_data in self.tracks.values():
				track_data.remove_attr("channel")
				
		self.segments.add(other.segments)
		
	def create_xml_element(self):
		# http://stackoverflow.com/a/22902367/1391325
		em = ElementMaker(nsmap={None: self.namespace })
		result = em("annotation")
		tracks_elem = etree.SubElement(result, self.qname_factory("tracks"))
		for track_id, track_data in sorted(self.tracks.items(), key=lambda k: natural_keys(k[0])):
			track_elem = etree.SubElement(tracks_elem, self.qname_factory("track"), attrib={"id" : track_id})
			sources_elem = etree.SubElement(track_elem, self.qname_factory("sources"))
			for track_source in sorted(track_data.sources_by_id.values(), key=lambda elem: natural_keys(elem.get("id"))):
				sources_elem.append(track_source)
		
		segments_elem = self.segments.create_xml_element()
		result.append(segments_elem)
			
		return result

class AnnotationParser(object):
	
	def __init__(self, id_prefix, qname_factory, namespace):
		self.id_prefix = id_prefix
		self.qname_factory = qname_factory
		self.namespace = namespace
		self.__tag_parsers = {self.qname_factory("tracks") : self.__parse_tracks, self.qname_factory("segments") : self.__parse_segments}
		self.__result = None
	
	def __call__(self, doc_tree):
		self.__result = AnnotationData(self.qname_factory, self.namespace, doc_tree.docinfo.encoding)
		tag_name = self.qname_factory("annotation")
		for child in doc_tree.iter(tag_name):
			self.__parse_annotation(child)		
			
		return self.__result
			
	def __parse_annotation(self, annot):
		for child in annot:
			tag_name = child.tag
			parser = self.__tag_parsers[tag_name]
			parser(child)
	
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
				source_id = self.id_prefix + source.get("id")
				source.set("id", source_id)
				track_sources.add(source)
				
class QNameStringFactory(object):
	
	def __init__(self, namespace):
		self.tag_qnames = {}
		self.__tag_prefix = "{" + namespace + "}"
		
	def __call__(self, tag_name):
		result = self.tag_qnames.get(tag_name)
		if not result:
			result = self.__tag_prefix + tag_name
			self.tag_qnames[tag_name] = result
		return result
	
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
		for segment in sorted(self.segments_by_id.values(), key=lambda elem: natural_keys(elem.get("id"))):
			result.append(segment)
			
		return result
	
class TrackSources(object):
	def __init__(self):
		self.sources_by_id = {}
		self.sources_by_channel = {}
		self.sources_by_href = {}

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)
		
	def add(self, source):
		source_id = source.get("id")
		self.sources_by_id[source_id] = source
		channel = source.get("channel")
		if not is_blank_or_none(channel):
			self.sources_by_channel[channel] = source
		href = source.get("href")
		if not is_blank_or_none(href):
			self.sources_by_href[href] = source
			
	def remove(self, source):
		source_id = source.get("id")
		del self.sources_by_id[source_id]
		channel = source.get("channel")
		if not is_blank_or_none(channel):
			del self.sources_by_channel[channel]
		href = source.get("href")
		if not is_blank_or_none(href):
			del self.sources_by_href[href]
			
	def remove_attr(self, attr):
		if attr == "id":
			raise ValueError("Cannot remove ID attribute: This is absolutely necessary.")
		else:
			for source in self.sources_by_id.values():
				# source.set(attr, None)
				try:
					del source.attrib["channel"]
				except KeyError:
					# Attr not present; just continue
					pass
		
def is_blank_or_none(str):
	return str is None or len(str) < 1 or str.isspace()

def natural_keys(text):
	'''
	alist.sort(key=natural_keys) sorts in human order
	
	:see: http://nedbatchelder.com/blog/200712/human_sorting.html
	:see: http://stackoverflow.com/a/5967539/1391325
	'''
	return [__atoi(c) for c in __DIGITS_PATTERN.split(text)]
	
def sanitize_dom_id(str):
	result = __WHITESPACE_PATTERN.sub('-', str)
	return escape(result)
	
def __atoi(text):
	'''
	:see: http://stackoverflow.com/a/5967539/1391325
	'''
	return int(text) if text.isdigit() else text
