import Pest;
import AST;

constant ALNUM = "abcdefghijklmnopqrstuvxyzåäö0123456789";

int main() {
  test("It should do something", lambda () {
    BufferFile b = BufferFile(Stdio.FakeFile(ALNUM));
    expect(b->current)->to_equal('a');
    expect(sizeof(b))->to_equal(strlen(ALNUM));
    expect(intp(b->read_byte()))->to_equal(true);
    expect(b->current)->to_equal('b');
    expect(b->read(2))->to_equal("bc");
    expect(b->peek_byte())->to_equal('e');
    expect(b->peek())->to_equal("e");
    expect(b->peek(3))->to_equal("efg");
    b->seek(15);
    expect(b->current)->to_equal('p');
  });

  test("It should read from start to end", lambda () {
    BufferFile b = BufferFile(Stdio.FakeFile(ALNUM));
    String.Buffer sb = String.Buffer();
    function add = sb->putchar;

    while (int t = b->read_byte()) {
      // werror("BYTE: %c [%[0]d]\n", t);
    }
  });
}
