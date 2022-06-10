private string _uri;
private string _text;
private int _version;

protected void create(mapping text_document) {
  this::_uri = text_document->uri;
  this::_text = text_document->text;
  this::_version = text_document->version;
}

public string `uri() {
  return _uri;
}

public string `text() {
  return _text;
}

public int `version() {
  return _version;
}

public void update(mapping change) {
  _version = change->textDocument->version;

  array(string) lines = _text/"\n";

  foreach (change->contentChanges, mapping edit) {
    werror("Update document text with: %O\n", edit);
    mapping range = edit->range;
    if (range->start->line != range->end->line) {
      werror("Fix multiline edit\n");
    } else {
      string sline = lines[range->start->line];
      werror("Line was: %s\n", sline);
      string newline = sline[..range->start->character - 1] + edit->text +
        sline[range->end->character..];

      if (!sizeof(newline)) {
        werror("Zonk line\n");
        lines[range->start->line] = 0;
      } else {
        lines[range->start->line] = newline;
      }
      werror("Line is: %s\n", newline);
    }
  }

  _text = (lines - ({ 0 })) * "\n";
}

protected string _sprintf(int how) {
  return sprintf(
    "%O(%s, v:%d) -> %d len",
    object_program(this), _uri, _version, sizeof(_text)
  );
}
