#charset utf-8
#pike __REAL_VERSION__

public Node make_node(
  program(Node) node_class,
  AST.Token.Location loc,
  void|mapping props
) {
  Node n = node_class(loc);

  foreach (props || ([]); string key; mixed val) {
    if (has_index(n, key)) {
      n[key] = val;
    }
  }

  return n;
}

public class Node {
  public AST.Token.Location location;

  protected void create(AST.Token.Location location) {
    this::location = location;
  }
}

public class Program {
  inherit Node;
  public array(object(Statement)) body = ({});

  protected string _sprintf(int t) {
    return sprintf("%O -> %O", object_program(this), body);
  }
}

public class Statement {
  inherit Node;
}

public class Modifier {
  inherit Statement;

  public AST.Token.Type type;
  public string name;
}

public class Annotation {
  inherit Statement;
}

public class EmptyStatement {
  inherit Statement;
}

public class Block {
  inherit Statement;
  public array(object(Statement)) body = ({});
}

public class Import {
  inherit Statement;
  public array(object(Identifier)) identifiers = ({});
  public string path;

  protected string _sprintf(int t) {
    if (path) {
      return sprintf("%O -> %q", object_program(this), path);
    }

    return sprintf("%O -> %O", object_program(this), identifiers);
  }
}

public class Bits {
  inherit Statement;
  int width;
}

public class IntRangeType {
  inherit Statement;
  //! Can represent something like:
  //! @code
  //! string(8bit)
  //! int(0..1)
  //! int(8)
  //! @endcode
  public Bits|int|array(int) range;

  protected string _sprintf(int t) {
    if (arrayp(range)) {
      if (undefinedp(range[0])) {
        return sprintf("..%d", range[1]);
      } else if (undefinedp(range[1])) {
        return sprintf("%d..", range[0]);
      }

      return sprintf("%d..%d", range[0], range[1]);
    } else {
      return sprintf("%d", range);
    }

    return sprintf("%dbits", range->width);
  }
}

public class IntrinsicType {
  inherit Statement;
  public string name;
}

public class IntrinsicStringType {
  inherit IntrinsicType;
  public IntRangeType width;

  protected string _sprintf(int t) {
    if (undefinedp(width)) {
      return sprintf("%O()", object_program(this));
    }

    return sprintf("%O(width: %O)", object_program(this), width || UNDEFINED);
  }
}

public class IntrinsicIntType {
  inherit IntrinsicType;
  public IntRangeType range;
}

public class Expression {
  inherit Node;
}

public class Identifier {
  inherit Expression;
  public string name;

  protected string _sprintf(int t) {
    return sprintf("%O(name: %q)", object_program(this), name);
  }
}

public class TypedIdentifier {
  inherit Identifier;
  public AST.Token.Type type;

  protected string _sprintf(int t) {
    return sprintf(
      "%O %O(name: %q)",
      AST.Token.type_to_string(type),
      object_program(this),
      name
    );
  }
}
