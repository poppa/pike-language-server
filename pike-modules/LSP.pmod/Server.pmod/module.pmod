constant VERSION = "0.1";
constant NAME = "Pike Language Server";

constant ERR_SERVER_NOT_INITIALIZED = -32002;
constant ERR_UNKNOWN = -32001;

public .Request parse_raw_request(Stdio.File input)
//! This expects that a JSON-RPC message is written to @[input] with
//! a `Content-Length` header.
//!
//! @throws An error if the body is not JSON parsable
{
  werror("parse_raw_request(pos@%d)\n", input->tell());
  String.Buffer buf = String.Buffer();

  while (string s = input->read(1)) {
    if (s == "") {
      break;
    }

    buf->add(s);

    if (s == "\r") {
      int pos = input->tell();

      if (input->read(3) == "\n\r\n") {
        buf->add("\n\r\n");
        break;
      }

      input->seek(pos, Stdio.SEEK_SET);
    }
  }

  if (sizeof(buf)) {
    string b = "GET / HTTP/1.0\r\n" + buf->get();
    mapping headers = _Roxen.HeaderParser()->feed(b)[2];
    string data = input->read((int)headers["content-length"]);
    return .Request(headers, data);
  }

  return UNDEFINED;
}
