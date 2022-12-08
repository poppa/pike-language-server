import Pest;
import LSP;

int main() {
  test("Basic...", lambda () {
    object d = Diagnostic.Diagnostic(0, 0);
    werror("D: %O\n", d);
  });
}
