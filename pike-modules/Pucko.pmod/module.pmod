// NOTE: This is an experiment to initally use the `pike -x dump` program to
//       do simple verification that code compiles.

public class Compiler {
  private string output_tmp_dir;

  protected void create() {
    assert_tmp_dir();
  }

  private void assert_tmp_dir() {
    if (!output_tmp_dir) {
      output_tmp_dir = combine_path(getenv("TMPDIR"), "pike-language-server");
    }

    if (!Stdio.exist(output_tmp_dir)) {
      mkdir(output_tmp_dir);
    }
  }

  private array(mapping) parse_message(string msg) {
    array(string) lines = String.trim(msg) / "\n";

    array(mapping) results = ({});
    mapping current;
    mapping prev;

    foreach (lines, string line) {
      if (has_value(line, "(not dumped)")) {
        break;
      }

      if (has_prefix(line, "####")) {
        if (current) {
          results += ({ copy_value(current) });
        }

        sscanf(line, "#### %s:", string filename);
        current = ([ "filename": filename, "errors": ({}), "warnings": ({}) ]);
        continue;
      }

      string fname, msg, type;
      int ln;

      mapping entry;

      if (sscanf(line, "%s:%d:(%1s)%s", fname, ln, type, msg) == 4) {
        entry = ([
          "file": fname,
          "line": ln,
          "msg": msg,
        ]);

        if (prev && prev->file == fname && prev->line == ln) {
          prev->msg += "\n" + msg;
        } else if (type == "E") {
          current->errors += ({ entry });
        } else if (type == "W") {
          current->warnings += ({ entry });
        } else {
          werror("Unhandled error type: %O\n", line);
        }

        prev = entry;
      } else {
        werror("Unhandled message: %s\n", line);
      }
    }

    if (current) {
      results += ({ current });
    }

    return results;
  }

  public void|array compile(string path) {
    if (!Stdio.exist(path)) {
      error("Input file %q doesn't exist!\n", path);
    }

    array(string) args = ({
      "pike",
      "-x",
      "dump",
      "-t",
      output_tmp_dir,
      path
    });

    mapping res = Process.run(args);

    if (res->stderr != "") {
      return parse_message(res->stderr);
    }
  }
}
