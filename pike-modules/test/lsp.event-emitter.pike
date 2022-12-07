import Pest;
import LSP;

int main() {
  test("It should register and handle on, once and off events", lambda () {
    EventEmitter emitter = EventEmitter();

    int cc = 0;

    Fn fn = Fn(lambda (int i) { expect(i)->to_equal(1); cc += 1; });
    Fn oncefn = Fn(lambda (int i) { expect(i)->to_equal(1); cc += 1; });

    emitter->on("init", fn);
    emitter->once("init", oncefn);

    emitter->emit("init", 1);
    emitter->emit("init", 1);
    emitter->emit("init", 1);

    emitter->off("init", fn);
    emitter->emit("init", 12);

    expect(fn)->to_have_been_called_n_times(3);
    expect(oncefn)->to_have_been_called_n_times(1);
    expect(cc)->to_equal(4);
  });

  test(
    "The same callback should not be added twice for the same event",
    lambda () {
      EventEmitter emitter = EventEmitter();

      Fn f = fn(lambda () {});
      emitter->on("test", f);
      emitter->on("test", f);
      emitter->emit("test");

      expect(f)->to_have_been_called_n_times(1);
    }
  );
}
