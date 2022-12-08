protected mapping cast(string how) {
  if (how != "mapping") {
    error("Can't cast %O to %O\n", object_program(this), how);
  }

  mapping m = ([]);

  foreach (indices(this), string key) {
    if (key[0] == '_') {
      continue;
    }

    mixed v = this[key];

    if (undefinedp(v)) {
      werror("%O->%O == zero_type\n", object_program(this), key);
    }

    key = to_camelcase(key);

    if (objectp(v)) {
      m[key] = (mapping) v;
    } else {
      m[key] = v;
    }
  }

  return m;
}

protected string to_camelcase(string s) {
  array(string) parts = s / "_";
  return (({ parts[0] }) + map(parts[1..], String.capitalize)) * "";
}
