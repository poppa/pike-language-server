public mixed deep_find_property(mapping in, string property) {
  if (has_index(in, property)) {
    return in[property];
  }

  foreach (values(in), mixed item) {
    if (mappingp(item)) {
      return this_function(item, property);
    }
  }

  return UNDEFINED;
}
