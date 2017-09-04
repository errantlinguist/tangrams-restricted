import mimetypes
import os
import re

XML_CONTENT_TYPE_PATTERN = re.compile(".*?/xml")


def walk_xml_files(*inpaths):
	for inpath in inpaths:
		for dirpath, _, filenames in os.walk(inpath, followlinks=True):
			for filename in filenames:
				resolved_path = os.path.join(dirpath, filename)
				mimetype = mimetypes.guess_type(resolved_path)[0]
				if mimetype is not None and XML_CONTENT_TYPE_PATTERN.match(mimetype):
					yield resolved_path
