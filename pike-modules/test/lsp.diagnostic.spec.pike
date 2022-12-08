import Pest;
import LSP;

int main() {
  test("Basic...", lambda () {
    object d = Diagnostic.Diagnostic();
    werror("D: %O\n", d);
  });
}
