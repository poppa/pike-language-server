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

public class WithModifier {
  public array(Modifier) modifier;
}

public class WithAttribute {
  public array(Attribute) attribute;
}

public class Program {
  inherit Node;
  inherit WithModifier;

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

  protected string _sprintf(int t) {
    return sprintf("%O(%s)", object_program(this), name);
  }
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
    } else if (objectp(range)) {
      return sprintf("%dbits", range->width);
    } else if (intp(range)) {
      return sprintf("%d", range);
    }

    return "";
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
  public IntRangeType range = UNDEFINED;

  protected string _sprintf(int t) {
    if (undefinedp(range)) {
      return sprintf("%O()", object_program(this));
    }

    return sprintf("%O(%O)", object_program(this), range);
  }
}

public class Expression {
  inherit Node;
}

public class Identifier {
  inherit Node;
  public string name;

  protected string _sprintf(int t) {
    return sprintf("%O(name: %q)", object_program(this), name);
  }
}

public class StringConstant {
  inherit Node;
  public string value;

  protected string _sprintf(int t) {
    return sprintf("%O(%q)", object_program(this), value);
  }
}

public class TypedIdentifier {
  inherit Identifier;
  inherit WithModifier;

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

public class TypeReference {
  inherit Statement;
}

public class FunctionDeclaration {
  inherit WithModifier;
  inherit WithAttribute;

  public Identifier name;
  public Node body; /* Block statement or Expression */
  public array(Node) params; /* FIXME: What should a param be? */
  public bool is_prototype;
  public IntrinsicType|TypeReference return_type;

  protected string _sprintf(int t) {
    return sprintf("%O(%O)", object_program(this), name);
  }
}

public class Attribute {
  inherit Statement;

  public string name;
  public void|StringConstant arg;

  protected string _sprintf(int t) {
    if (arg) {
      return sprintf("%O(%s: %q)", object_program(this), name, arg->value);
    }
    return sprintf("%O(%s)", object_program(this), name);
  }
}
