int main(int argc, array(string) argv) {
  werror("Server start called: %O\n", argv);

  string pipe;
  string mode;

  foreach (argv, string arg) {
    if (sscanf(arg, "--pipe=%s", string sock) == 1) {
      pipe = sock;
      mode = "pipe";
      break;
    } else if (arg == "--stdio") {
      mode = "stdio";
      break;
    }
  }

  if (mode == "stdio") {
    LSP.Server.Stdio server = LSP.Server.Stdio();
    server->start();
  }

  return -1;
}
