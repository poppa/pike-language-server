public class FakeBaseConnection {
  inherit LSP.Server.Base;

  protected mapping init_params = ([]);
  protected bool is_initialized = true;
}

public class FakeStdioConnection {
  inherit LSP.Server.Stdio;

  public void start(Stdio.File pipe) {
    ::start_handler(pipe, pipe);
  }
}
