private mapping(string:object(Event)) events = ([]);

public enum Type {
  ON,
  ONCE,
}

public this_object on(string event_name, function cb) {
  return add_listener(ON, event_name, cb);
}

public this_object once(string event_name, function cb) {
  return add_listener(ONCE, event_name, cb);
}

public this_object off(string event_name, function cb) {
  remove_listener(ON, event_name, cb);
  remove_listener(ONCE, event_name, cb);
  return this;
}

public void emit(string event_name, mixed ... args) {
  array(string) keys = ({
    sprintf("%s:%d", event_name, ONCE),
    sprintf("%s:%d", event_name, ON)
  });

  foreach (keys, string key) {
    if (Event e = events[key]) {
      foreach (e->callbacks, function cb) {
        cb(@args);
      }

      if (e->type == ONCE) {
        e->callbacks = UNDEFINED;
        m_delete(events, key);
      }
    }
  }
}

public this_object add_listener(Type type, string event_name, function cb) {
  string key = sprintf("%s:%d", event_name, type);
  events[key] = events[key] || Event(type);
  events[key]->add_callback(cb);
  return this;
}

public this_object remove_listener(
  Type type,
  string event_name,
  void|function cb
) {
  string key = sprintf("%s:%d", event_name, type);

  if (!has_index(events, key)) {
    return this;
  }

  if (!cb) {
    m_delete(events, key);
  } else {
    events[key]->remove_callback(cb);
  }

  return this;
}

protected class Event {
  public Type type;
  public array(function) callbacks = ({});

  protected void create(Type type) {
    this::type = type;
  }

  public this_object add_callback(function f) {
    if (!has_value(callbacks, f)) {
      callbacks += ({ f });
    }

    return this;
  }

  public bool remove_callback(function f) {
    int len = sizeof(callbacks);
    callbacks -= ({ f });
    return sizeof(callbacks) < len;
  }
}
