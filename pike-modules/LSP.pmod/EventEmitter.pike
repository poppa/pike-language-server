private mapping(string:multiset(function)) events = ([]);

public this_object on(string event_name, function cb) {
  if (!events[event_name]) {
    events[event_name] = (<>);
  }

  if (events[event_name][cb]) {
    return this;
  }

  events[event_name] += (< cb >);

  return this;
}

public this_object once(string event_name, function cb) {
  function proxy;
  proxy = lambda (mixed ... args) {
    off(event_name, proxy);
    cb(@args);
  };

  return on(event_name, proxy);
}

public this_object off(string event_name, function cb) {
  if (events[event_name][cb]) {
    events[event_name] -= (< cb >);

    if (sizeof(events[event_name]) == 0) {
      m_delete(events, event_name);
    }
  }

  return this;
}

public void emit(string event_name, mixed ... args) {
  multiset(function) cbs = events[event_name] || (<>);

  foreach ((array) cbs, function cb) {
    cb(@args);
  }
}
