import Pest;

class MyArgs {
  inherit Arg.Options;
  Opt file = HasOpt("--file")|HasOpt("-f");
  Opt test = HasOpt("--test")|HasOpt("-t");
}

int main(int argc, array(string) argv) {
  MyArgs a = MyArgs(argv);

  GlobArg file_globs;
  GlobArg test_globs;

  if (a->file) {
    file_globs = a->file / ",";
  }

  if (a->test) {
    test_globs = a->test / ",";
  }

  run_test(__DIR__, file_globs, test_globs);
}
