#charset utf-8
#pike __REAL_VERSION__

#include "ast.h"

protected Stdio.File source;
protected string buffer;
protected int cursor;
protected int buf_size;
protected int buf_len;

protected void create(Stdio.File|Stdio.FakeFile source, int buffer_size) {
  cursor = source->tell();
  this::source = source;
  this::buf_size = buffer_size;
  string raw = source->read();
  this::buffer = utf8_to_string(raw);

  source->seek(cursor, Stdio.SEEK_SET);
  buf_len = strlen(this::buffer);
}

protected variant void create(Stdio.File|Stdio.FakeFile source) {
  create(source, 1024 * 10);
}

public string read(int len) {
  int start_pos = cursor;
  int end_pos = start_pos + len - 1;

  if (end_pos > buf_len) {
    end_pos = buf_len;
  }

  if (!buffer || cursor + len > buf_len) {
    return UNDEFINED;
  }

  cursor = end_pos + 1;
  return buffer[start_pos .. end_pos];
}

public int read_byte() {
  if (cursor >= buf_len) {
    return UNDEFINED;
  }
  int v = buffer[cursor];
  cursor += 1;
  return v;
}

public int `current() {
  if (cursor >= buf_len) {
    return UNDEFINED;
  }

  return buffer[cursor];
}

public string peek(int n) {
  if (cursor + n >= buf_len) {
    return UNDEFINED;
  }

  return buffer[cursor + 1 .. cursor + n];
}

public variant string peek() {
  return peek(1);
}

public int peek_byte(int skip) {
  if (cursor + skip > buf_len) {
    return UNDEFINED;
  }

  return buffer[cursor + skip];
}

public variant int peek_byte() {
  return peek_byte(1);
}

public int tell() {
  return cursor;
}

public int seek(int pos, string whence) {
  int next_pos = whence == Stdio.SEEK_SET ? pos : cursor + pos;

  if (next_pos < 0) {
    cursor = 0;
  } else if (next_pos >= buf_len) {
    cursor = buf_len -1 ;
  } else {
    cursor = next_pos;
  }

  return cursor;
}

public variant int seek(int pos) {
  return seek(pos, Stdio.SEEK_SET);
}

public bool is_fakefile() {
  return object_program(source) == Stdio.FakeFile;
}

protected int _sizeof() {
  return buf_len;
}
