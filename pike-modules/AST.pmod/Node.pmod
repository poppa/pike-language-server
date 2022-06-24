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
  string path;

  protected string _sprintf(int t) {
    if (path) {
      return sprintf("%O -> %q", object_program(this), path);
    }

    return sprintf("%O -> %O", object_program(this), identifiers);
  }
}

public class IntrinsicType {
  inherit Statement;
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
