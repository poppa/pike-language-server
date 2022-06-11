//! A tagging type for string properties that are actually URIs.
public typedef string DocumentUri;

public typedef Content|ContentChange ContentChangeEvent;

public Document create_document(
  DocumentUri uri,
  string language_id,
  int version,
  string content
)
//! Creates a new text document.
//!
//! @param uri The document's uri.
//! @param language_id  The document's language Id.
//! @param version The document's initial version number.
//! @param content The document's content.
{
  return Document(uri, language_id, version, content);
}

public variant Document create_document(mapping document)
//! Creates a document from a JSON-RPC message
{
  return create_document(
    document->uri,
    document->languageId,
    document->version,
    document->text
  );
}

public class Document
//! A simple text document
{
  private string _uri;
  private string _language_id;
  private int _version;
  private string _text;

  protected void create(
    string uri,
    string language_id,
    int version,
    string text
  ) {
    this::_uri = uri;
    this::_language_id = language_id;
    this::_version = version;
    this::_text = text;
  }

  public string `uri()
  //! The associated URI for this document. Most documents have the
  //! __file__-scheme, indicating that they represent files on disk. However,
  //! some documents may have other schemes indicating that they are not
  //! available on disk.
  {
    return _uri;
  }

  public string `language_id()
  //! The identifier of the language associated with this document.
  //! In this case it will (propbably) always be `pike`
  {
    return _language_id;
  }

  public string `text()
  //! The text of this document
  {
    return _text;
  }

  public int `version()
  //! The version number of this document (it will increase after each
  //! change, including undo/redo).
  {
    return _version;
  }

  public int `line_count()
  //! The number of lines in this document.
  {
    return sizeof(_text / "\n");
  }

  public string get_text(Range range)
  //! Get a range of the text of this document.
  //!
  //! @param range
  //!  An range within the document to return.
  //!  If no range is passed, the full content is returned.
  //!  Invalid range positions are adjusted as described in @[Position.line]
  //!  and @[Position.character].
  //!  If the start range position is greater than the end range position,
  //!  then the effect of @[get_text()] is as if the two positions were swapped.
  //!
  //! @returns
  //!  A substring of the text
  {

  }

  public Position position_at(int offset)
  //! Converts a zero-based offset to a position.
  //!
  //! @param offset A zero-based offset.
  //! @returns A valid @[Position].
  {

  }

  public this_object update(array(ContentChangeEvent) changes, int version)
  //! Updates the document by modifying its content.
  //!
  //! @param changes the changes to apply to the document.
  //! @param version the changes version for the document.
  //!
  //! @returns The object being called
  {

    return this;
  }

  public this_object apply_edits(array(TextEdit) edits)
  //! Upadte the document with @[edits]
  //!
  //! @param edits The edits to apply to the document
  //!
  //! @returns The object being called
  {

    return this;
  }

  protected string _sprintf(int how) {
    return sprintf("%O(%s, v:%d)", object_program(this), _uri, _version);
  }
}

public class Position
//! Position in a text document expressed as zero-based line and character
//! offset.
//! The offsets are based on a UTF-16 string representation. So a string of the
//! form `a𐐀b` the character offset of the character `a` is 0, the character
//! offset of `𐐀` is 1 and the character offset of b is 3 since `𐐀` is
//! represented using two code units in UTF-16.
//!
//! Positions are line end character agnostic. So you can not specify a position
//! that denotes `\r|\n` or `\n|` where `|` represents the character offset.
{
  //! Line position in a document (zero-based).
  //! * If a line number is greater than the number of lines in a document, it
  //!   defaults back to the number of lines in the document.
  //! * If a line number is negative, it defaults to 0.
  public int line;


  //! Character offset on a line in a document (zero-based). Assuming that the
  //! line is represented as a string, the `character` value represents the gap
  //! between the `character` and `character + 1`.
  //!
  //! If the character value is greater than the line length it defaults back
  //! to the line length.
  //!
  //! If a line number is negative, it defaults to 0.
  public int character;
}

public class Range
//! A range in a text document expressed as (zero-based) start and end
//! positions.
//!
//! If you want to specify a range that contains a line including the line
//! ending character(s) then use an end position denoting the start of the next
//! line.
//! For example:
//! @code
//! ([
//!   start: ([ line: 5, character: 23 ])
//!   end : ([ line 6, character : 0 ])
//! ])
//! @endcode
{
  //! The range's start position
  public Position start;

  //! The range's end position.
  public Position end;
}

public class TextEdit
//! A text edit applicable to a text document
{
  //! The range of the text document to be manipulated. To insert
  //! text into a document create a range where start == end.
  public Range range;

  //! The string to be inserted. For delete operations use an empty string.
  public string new_text;
}

public class Content {
  public string text;
}

public class ContentChange {
  inherit Content;
  public Range range;
}
