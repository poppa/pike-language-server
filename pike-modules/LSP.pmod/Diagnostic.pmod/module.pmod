
import LSP.TextDocument;

#define OPTIONAL(T) T|void

public enum Severity
// The diagnostic severity
{
  //! Reports an error
  Error = 1,

  //! Reports a warning
  Warning,

  //! Reports an information
  Information,

  //! Reports a hint
  Hint,
}

public enum Tag {
  //! Unused or unnecessary code.
	//!
	//! Clients are allowed to render diagnostics with this tag faded out
	//! instead of having an error squiggle.
  Unnecessary = 1,

  //! Deprecated or obsolete code.
	//!
	//! Clients are allowed to rendered diagnostics with this tag strike through.
  Deprecated,
}

public class Diagnostic
//! Represents a diagnostic, such as a compiler error or warning.
//! Diagnostic objects are only valid in the scope of a resource.
{
  //! The range at which the message applies.
  public Range range;

  //! The diagnostic's message.
  public string message;

  //! The diagnostic's severity. Can be omitted. If omitted it is up to the
	//! client to interpret diagnostics as error, warning, info or hint.
  public OPTIONAL(Severity) severity;

  //! The diagnostic's code, which might appear in the user interface.
  public OPTIONAL(int|string) code;

  //! An optional property to describe the error code.
  public OPTIONAL(CodeDescription) code_description;

  //! A human-readable string describing the source of this diagnostic, e.g.
  //! 'typescript' or 'super lint'.
  public OPTIONAL(string) source;

  //! Additional metadata about the diagnostic.
  public OPTIONAL(Tag) tags;

  //! An array of related diagnostic information, e.g. when symbol-names within
  //! a scope collide all definitions can be marked via this property.
  public OPTIONAL(RelatedInformation) related_information;

  //! A data entry field that is preserved between a
	//! `textDocument/publishDiagnostics` notification and
	//! `textDocument/codeAction` request.
  public OPTIONAL(mixed) data;

  protected void create(Range range, string message) {
    this::range = range;
    this::message = message;
  }
}

public class CodeDescription
//! Structure to capture a description for an error code.
{
  //! An URI to open with more information about the diagnostic error.
  public DocumentUri uri;
}

public class RelatedInformation
//! Represents a related message and source code location for a diagnostic.
//! This should be used to point to code locations that cause or are related to
//! a diagnostics, e.g when duplicating a symbol in a scope.
{
  //! The location of this related diagnostic information.
  public Location location;

  //! The message of this related diagnostic information.
  public string message;
}
