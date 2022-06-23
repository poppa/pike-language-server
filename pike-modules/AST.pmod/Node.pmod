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
}

public class Statement {
  inherit Node;
}

public class EmptyStatement {
  inherit Statement;
}

public class BlockStatement {
  inherit Statement;
  public array(object(Statement)) body = ({});
}

public class ImportStatement {
  inherit Statement;
  public array(object(Identifier)) identifiers = ({});
}

public class TypeStatement {
  inherit Statement;
}

public class Expression {
  inherit Node;
}

public class Identifier {
  inherit Expression;
  public string name;
}

public class TypedIdentifier {
  inherit Identifier;
  public AST.Token.Type type;
}
