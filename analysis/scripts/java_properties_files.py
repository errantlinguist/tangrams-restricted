import os
import re

PROPERTIES_FILEPATH_PATTERN = re.compile(".+?\.properties$")


def parse_properties(lines):
	result = {}
	stripped_lines = (line.strip() for line in lines)
	semantic_lines = (line for line in stripped_lines if line and not line.startswith('#'))
	props = (line.split('=', maxsplit=1) for line in semantic_lines)
	for prop in props:
		prop_name = prop[0]
		prop_value = prop[1]
		__set_prop_value(result, prop_name, prop_value)
	return result


def walk_properties_files(*inpaths):
	for inpath in inpaths:
		for dirpath, _, filenames in os.walk(inpath, followlinks=True):
			for filename in filenames:
				if PROPERTIES_FILEPATH_PATTERN.match(filename):
					resolved_path = os.path.join(dirpath, filename)
					yield resolved_path


def __fetch_prop_group(props, prop_name):
	try:
		result = props[prop_name]
	except KeyError:
		result = {}
		props[prop_name] = result
	return result


def __set_nested_prop_value(props, prop_name_components, prop_value):
	terminal_prop_group = props
	name_component_iter = iter(prop_name_components)
	next_name_component = next(name_component_iter)
	reached_terminal_group = False
	while not reached_terminal_group:
		current_name_component = next_name_component
		try:
			next_name_component = next(name_component_iter)
			# The current component is not terminal; Get a nested group for it
			terminal_prop_group = __fetch_prop_group(terminal_prop_group, current_name_component)
		except StopIteration:
			# The current component is terminal; Insert the given value at this level
			terminal_prop_group[current_name_component] = prop_value
			reached_terminal_group = True


def __set_prop_value(props, prop_name, prop_value):
	prop_name_components = prop_name.split('.')
	if len(prop_name_components) < 2:
		props[prop_name] = prop_value
	else:
		__set_nested_prop_value(props, prop_name_components, prop_value)
