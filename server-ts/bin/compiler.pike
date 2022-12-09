#require constant(Pucko)

int main(int argc, array(string) argv) {
  if (argc < 2) {
    werror("Missing required argument [path]\n");
    exit(1);
  }

  string path = argv[1];

  if (!Stdio.exist(path)) {
    werror("%q doesn't exist\n", path);
    exit(1);
  }

  array|void messages = Pucko.Compiler()->compile(path);

  if (sizeof(messages?->errors || ({})) || sizeof(messages?->warnings || ({}))) {
    write("%s", Standards.JSON.encode(messages));
  }
}
