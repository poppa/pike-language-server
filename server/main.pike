int main(int argc, array(string) argv) {
  werror("Server start called: %O\n", argv);

  string use_pipe;
  bool use_stdio;

  foreach (argv, string arg) {
    if (sscanf(arg, "--pipe=%s", string sock) == 1) {
      use_pipe = sock;
      break;
    } else if (arg == "--stdio") {
      use_stdio = true;
      break;
    }
  }

  if (use_stdio) {
    LSP.Server.Stdio server = LSP.Server.Stdio();
    server->start();
    werror("Post start, now in handler thread land\n");
  }

  return -1;
}
