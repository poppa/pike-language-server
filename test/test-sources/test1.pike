#include "rep.h"

// Some commonly used xpath-like XML functions.

Parser.XML.Tree.SimpleNode find_node(Parser.XML.Tree.SimpleNode node,
                                     string path)
{
  //  Just get first child that matches the given path
  foreach((path / "/") - ({ "" }), string segment) {
    array(string) tags = node->get_children()->get_tag_name();
    int pos = search(tags, segment);
    if (pos >= 0)
      node = node[pos];
    else
      return 0;
  }
  return node;
}

array(Parser.XML.Tree.SimpleNode) find_nodes(Parser.XML.Tree.SimpleNode node,
                                             string|array path)
{
  //  Find all children at each level that match each path segment. When
  //  we are called recursively the path will already be in array form.
  array(string) segments = arrayp(path) ? path : (path / "/") - ({ "" });

  //  Always return an array even if no matches are found
  array(Parser.XML.Tree.SimpleNode) res = ({ });

  if (sizeof(segments)) {
    res = filter(node->get_children(),
                 lambda(Parser.XML.Tree.SimpleNode n) {
                   return
                     (n->get_node_type() == Parser.XML.Tree.XML_ELEMENT) &&
                     (< n->get_tag_name(), "*" >)[segments[0]];
                 });

    //  If we've got any result and there are additional path segments
    //  left to process we run them all in parallel.
    if (sizeof(res) && sizeof(segments) > 1)
      res = Array.flatten(map(res, find_nodes, segments[1..]));
  }
  return res;
}

string find_string_val(Parser.XML.Tree.SimpleNode node, string path)
{
  //  If path ends with attribute we stop one level above
  if (has_value(path, "@")) {
    [path, string attr] = path / "@";
    node = find_node(node, path);
    return node && node->get_attributes()[attr];
  } else {
    node = find_node(node, path);
    return node && node->value_of_node();
  }
}

string find_xml_val(Parser.XML.Tree.SimpleNode node, string path)
{
  //  Find node and generate string from subtree
  node = find_node(node, path);
  return
    node &&
    map(node->get_children(), Sitebuilder.my_html_of_node, 0) * "";
}


class JSONPath(array|mapping root_obj, string expr, void|mapping args)
{
  //  -------------------------------------------------------------------
  //  Basic expression evaluation based on RXML implementation in Roxen
  //  WebServer.

  SExprCompileHandler sexpr_handler = SExprCompileHandler();
  mapping(string:program) compiled_exprs = ([ ]);

  private object sexpr_funcs = class {
      // A class for the special functions in sexpr_constants. This is
      // to give these function proper names, since those names can
      // occur in the compiler errors that are shown to users.

      int INT (void|mixed x) {
        return intp (x) || floatp (x) || stringp (x) ? (int) x : 0;
      }

      float FLOAT (void|mixed x) {
        return intp (x) || floatp (x) || stringp (x) ? (float) x : 0.0;
      }

      string STRING (void|mixed x) {
        return intp (x) || floatp (x) || stringp (x) ? (string) x : "";
      }

      array regexp_split (string regexp, string data) {
        Regexp.PCRE.Plain re;
        if (mixed err = catch (re = Regexp.PCRE.Widestring (regexp)))
          return 0;
        return re->split2 (data) || 0;
      }

      float exp(void|int x) {
        return predef::exp(intp(x) ? (float) x : x);
      }

      float log(void|mixed x) {
        return predef::log(intp(x) ? (float) x : x);
      }

      int floor(void|mixed x) {
        return (int) predef::floor(intp(x) ? (float) x : x);
      }

      int ceil(void|mixed x) {
        return (int) predef::ceil(intp(x) ? (float) x : x);
      }

      int round(void|mixed x) {
        return (int) predef::round(intp(x) ? (float) x : x);
      }

      mixed ai(array a, int i) {
        return a[i];
      }
    }();

  private mapping(string:mixed) sexpr_constants = ([
    "this": 0,
    "this_function": 0,
    "this_program": 0,

    // The (function) casts below is to avoid very bulky types that
    // causes the compiler to take extra time to compile this file.
    "`+": (function) `+,
    "`-": (function) `-,
    "`*": (function) `*,
    "`/": (function) `/,
    "`%": (function) `%,
    "`!": (function) `!,
    "`!=": (function) `!=,
    "`&": (function) `&,
    "`|": (function) `|,
    "`^": (function) `^,
    "`<": (function) `<,
    "`>": (function) `>,
    "`==": (function) `==,
    "`<=": (function) `<=,
    "`>=": (function) `>=,

    "arrayp": arrayp,
    "floatp": floatp,
    "functionp": functionp,
    "intp": intp,
    "mappingp": mappingp,
    "multisetp": multisetp,
    "objectp": objectp,
    "programp": programp,
    "stringp": stringp,
    "undefinedp": undefinedp,
    "zero_type": zero_type,

    "has_index": has_index,
    "has_prefix": has_prefix,
    "has_suffix": has_suffix,
    "has_value": has_value,
    "indices": indices,
    "values": values,

    "equal": equal,
    "sizeof": sizeof,
    "strlen": strlen,
    "pow":pow,
    "exp": sexpr_funcs->exp,
    "log": sexpr_funcs->log,
    "abs": abs,
    "max": max,
    "min": min,
    "round": sexpr_funcs->round,
    "floor": sexpr_funcs->floor,
    "ceil": sexpr_funcs->ceil,
    "search": search,
    "reverse": reverse,
    "uniq": Array.uniq,
    "regexp_split": sexpr_funcs->regexp_split,
    "basename": basename,
    "dirname": dirname,
    "combine_path": combine_path_unix,

    "INT": sexpr_funcs->INT,
    "FLOAT": sexpr_funcs->FLOAT,
    "STRING": sexpr_funcs->STRING,

    //  Array indexing which replaces arr[i] since "[" and "]" are not
    //  allowed inside "(...)" and "?(...)" expressions.
    "ai": sexpr_funcs->ai
  ]);

  private class SExprCompileHandler
  {
    string errmsg;

    mapping(string:mixed) get_default_module() {
      return sexpr_constants;
    }

    mixed resolv(string id, void|string fn, void|string ch) {
      // Only the identifiers in sexpr_constants are allowed.
      throw(Error.mkerror("Unknown identifier: "+ id));
    }

    void compile_error (string a, int b, string c) {
      if (!errmsg) errmsg = c;
    }

    int compile_exception (mixed err) {
      return 0;
    }
  }

  string compile_expr(string expr)
  {
    array(string) split = expr / "\"";
    for (int i = 0; i < sizeof(split); ) {
      string s = split[i];
      if (i > 0) {
        string p = split[i - 1];
        int b;
        for (b = sizeof (p) - 1; b >= 0; b--)
          if (p[b] != '\\')
            break;
        if (!((sizeof (p) - b) & 1)) {
          i++;
          continue;
        }
      }
      if (has_value(s, "lambda") ||
          has_value(s, "program") ||
          has_value(s, "object") ||
          sscanf(s, "%*[^;{[]%*c") > 1) {
        return "";
      }
      split[i] = s;
      i += 2;
    }

    string new_expr = split * "\"";
    string trimmed_new_expr =
        has_prefix(new_expr, "?(") ?
        new_expr[2..<1] : new_expr[1..<1];
    if (!compiled_exprs[trimmed_new_expr]) {
      if (mixed err = catch {
          string code_wrapper =
            "mixed _fn(array|mapping _v, array|mapping _root_obj) {"
            "  return (" + replace(trimmed_new_expr,
                                   ({ "@", "$" }),
                                   ({ "_v", "_root_obj" }) ) + ");"
            "}";
          compiled_exprs[trimmed_new_expr] =
            compile_string(code_wrapper, 0, sexpr_handler);
        }) {
        string msg =
          sprintf("Error in expression %s:, %s\n",
                  expr, sexpr_handler->errmsg || describe_error(err));
        throw(Error.mkerror(msg));
      }
    }

    return new_expr;
  }

  //  -------------------------------------------------------------------
  //  Pike adaption of jsonpath-0.8.0.js originally by Stefan Goessner
  //  (goessner.net) with minor alterations.
  //
  //  See https://goessner.net/articles/JsonPath/ for docs and samples.

  string result_type = (args && args->resultType) || "VALUE";
  array result = ({ });

  string jp_normalize(string expr)
  {
    mapping subst = ([ ]);
    int skipcount = 0;
    while (sscanf(expr, "%" + skipcount + "s%s%[\[']%s%[\]']%s",
                  string skip, string pre, string open, string subexpr,
                  string close, string post) == 6) {
      if (!sizeof(subexpr) && !sizeof(post))
        break;
      if (has_suffix(subexpr, ")") &&
          (has_prefix(subexpr, "?(") || has_prefix(subexpr, "("))) {
        string key = sprintf("#%03d", sizeof(subst) + 1);
        subst[key] = compile_expr(subexpr);
        expr = skip + pre + open + key + close + post;
        skipcount = sizeof(skip + pre + open + key + close);
      } else {
        skipcount = sizeof(skip + pre + open + subexpr + close);
      }
    }
    expr = replace(expr, ({ "'.'", "['" }), ({ ";", ";" }) );
    expr = replace(expr, ({ ".", "[" }), ({ ";", ";" }) );
    expr = replace(expr, ";;;", ";..;");
    expr = replace(expr, ";;", ";..;");
    expr = replace(expr, "']", "");
    expr = replace(expr, "]", "");
    if (has_suffix(expr, ";") || has_suffix(expr, "'"))
      expr = expr[..<1];
    if (has_prefix(expr, "$;"))
      expr = expr[2..];
    expr = replace(expr, subst);
    return expr;
  }

  string jp_asPath(string path)
  {
    string p = "$";
    foreach ((path / ";")[1..], string seg) {
      if ((string)(int) seg != seg)
        seg = "'" + seg + "'";
      p += "[" + seg + "]";
    }
    return p;
  }

  void jp_store(string path, array|mapping val)
  {
    if (path)
      result += ({ result_type == "PATH" ? jp_asPath(path) : val });
  }

  void jp_trace(string expr, array|mapping val, string path)
  {
    if (sizeof(expr)) {
      string|array(string) x = expr / ";";
      string loc = x[0];
      x = x[1..] * ";";
      if (arrayp(val) && (loc == (string)(int)loc)) {
        int loc_num = (int)loc;
        if ((loc_num > -(sizeof(val) + 1)) &&
            (loc_num < sizeof(val))) {
          jp_trace(x, val[loc_num], path + ";" + loc);
        }
      } else if (mappingp(val) && !zero_type(val[loc])) {
        jp_trace(x, val[loc], path + ";" + loc);
      } else if (loc == "*") {
        jp_walk(loc, x, val, path,
                lambda(string|int key, string l, string x, array|mapping v,
                       string p) {
                  jp_trace(key + ";" + x, v, p);
                });
      } else if (loc == "..") {
        jp_trace(x, val, path);
        jp_walk(loc, x, val, path,
                lambda(string|int key, string l, string x, array|mapping v,
                       string p) {
                  if (arrayp(v[key]) || mappingp(v[key]))
                    jp_trace("..;" + x, v[key], p + ";" + key);
                });
      } else if (has_prefix(loc, "(") && has_suffix(loc, ")")) {   // [(expr)]
        jp_trace(jp_eval(loc[1..<1], val, (path / ";")[-1]) + ";" + x,
                 val, path);
      } else if (has_prefix(loc, "?(") && has_suffix(loc, ")")) {  // [?(expr)]
        jp_walk(loc[2..<1], x, val, path,
                lambda(string|int key, string l, string x, array|mapping v,
                       string p) {
                  if (jp_eval(l, v[key], key))
                    jp_trace(key + ";" + x, v, p);
                });
      } else if (has_value(loc, ",")) {  //  [name1,name2,...]
        loc = replace(loc, "','", ",");
        foreach (loc / ",", string idx) {
          jp_trace(idx + ";" + x, val, path);
        }
      } else if (sscanf(loc, "%*d:%*d:%*d")) {  //  [start:end:step]
        jp_slice(loc, x, val, path);
      }
    } else {
      jp_store(path, val);
    }
  }

  void jp_walk(string loc, string expr, array|mapping val, string path,
               function f)
  {
    if (arrayp(val)) {
      foreach (val; int idx; )
        f(idx, loc, expr, val, path);
    } else if (mappingp(val)) {
      foreach (val; string key; )
        f(key, loc, expr, val, path);
    }
  }

  void jp_slice(string loc, string expr, array|mapping val, string path)
  {
    if (arrayp(val)) {
      int start = 0;
      int end = sizeof(val);
      int step = 1;
      int len = end;
      sscanf(loc, "%d:%d:%d", start, end, step);
      start = (start < 0) ? max(0, start + len) : min(len, start);
      end =   (end < 0)   ? max(0, end + len)   : min(len, end);
      for (int i = start; i < end; i+= step)
        jp_trace(i + ";" + expr, val, path);
    }
  }

  mixed jp_eval(string x, array|mapping _v, string _vname)
  {
    mixed err = catch {
      if (program prog = compiled_exprs[x]) {
        mixed res = prog()->_fn(_v, root_obj);
        return res;
      }
    };
    string msg = sprintf("Error in expression: %s\n", describe_error(err));
    throw(Error.mkerror(msg));
  }

  array run()
  {
    if (expr && root_obj && (< "VALUE", "PATH" >)[result_type]) {
      expr = jp_normalize(expr);
      jp_trace(expr, root_obj, "$");
      return sizeof(result) ? result : UNDEFINED;
    }
    return UNDEFINED;
  }
}

array json_path(array|mapping json, string expr, void|mapping args)
{
  if (mixed err = catch {
      JSONPath jp = JSONPath(json, expr, args);
      return jp->run();
    }) {
    report_error("REP.Utils.json_path: Expression error: %s\n",
                 describe_backtrace(err));
  }
  return UNDEFINED;
}


//!  Accepts a simple pattern on any of these forms:
//!
//!    ("*" | tag) (id | cls | attr)*
//!
//!  ...meaning it will cover e.g. these patterns:
//!
//!    *
//!    tag
//!    tag#id123.cls1
//!    tag.cls1.cls2[attr="foo"]
//!    *[attr1^="bar"][attr2].cls[attr3$="fie"]
//!    *.cls1.cls2#id234
//!
//!  The attribute parts support CSS attribute selectors like these:
//!
//!    [attr]
//!    [attr="value"]
//!    [attr~="value"]
//!    [attr|="value"]
//!    [attr^="value"]
//!    [attr$="value"]
//!    [attr*="value"]
//!
//!  We only support value strings that are quoted with single or double
//!  quotes. A tag name or wildcard must be found at the beginning of the
//!  pattern.
class SimpleCSSPattern
{
  constant valid_ops = (< "=", "~=", "|=", "^=", "$=", "*=" >);

  //  The return value is a mapping:
  //
  //    ([ "element" : "tag",
  //       "id"      : "idref"
  //       "classes" : ({ "cls1", "cls2" }),
  //       "attrs"   : ({ ({ "attr1", 0, 0 }) ,
  //                      ({ "attr2", "=", "val1" }),
  //                      ({ "attr3", "|=", "val2" }) })
  //    ])
  //
  //  The "id" entry may be 0, and the "classes" and "attrs" arrays may be
  //  empty.
  mapping(string:string|array) data;

  int(0..1) valid()
  {
    return data && 1;
  }

  void create(string pattern)
  {
    mapping(string:string|array) new_data = ([ "element" : 0,
                                               "id"      : 0,
                                               "classes" : ({ }),
                                               "attrs"   : ({ }) ]);

    //  Skip tag or wildcard
    string token;
    pattern = String.normalize_space(pattern);
    if ((sscanf(pattern, "%s%*[#.[]", token) != 2) || !sizeof(token) ||
        has_value(token, " ")) {
      return;
    }
    new_data->element = token;
    pattern = pattern[sizeof(token)..];

    while (sizeof(pattern)) {
      //  We expect either "#idref", ".cls" or a "[...]" block
      switch (pattern[0..0]) {
      case "#":
        if ((sscanf(pattern, "#%s%*[#.[]", token) != 2) ||
            has_value(token, " ")) {
          return;
        }
        new_data->id = token;
        pattern = pattern[1 + sizeof(token)..];
        break;

      case ".":
        if ((sscanf(pattern, ".%s%*[#.[]", token) != 2) ||
            has_value(token, " ")) {
          return;
        }
        new_data->classes += ({ token });
        pattern = pattern[1 + sizeof(token)..];
        break;

      case "[":
        //  Scan attribute name and relation op. We also need to support the
        //  form "[attr]" with varying degrees of whitespace.
        string attr, op, val, ws;
        if (sscanf(pattern, "[%[ ]%s%[=~|^$* ]", ws, attr, op) != 3)
          return;
        int op_width = sizeof(op);
        op = String.trim_whites(op);
        if (!sizeof(op) || has_value(attr, "]")) {
          //  The attr string may contain a trailing "]" (plus more data)
          //  which we must adjust for.
          if (has_prefix(attr, "]"))
            return 0;
          attr = (attr / "]")[0];
          new_data->attrs += ({ ({ attr, 0, 0 }) });
          pattern = pattern[1 + sizeof(ws) + sizeof(attr)..];
          pattern = String.trim_whites(pattern);
        } else {
          if (!sizeof(attr) || !valid_ops[op])
            return;
          pattern  = pattern[1 + sizeof(ws) + sizeof(attr) + op_width..];

          //  Check for quote char and scan remaining parts
          string quote = pattern[0..0];
          if (quote != "\"" && quote != "'")
            return;
          if (sscanf(pattern, quote + "%s" + quote + "%[ ]", val, ws) != 2)
            return;
          new_data->attrs += ({ ({ attr, op, val }) });
          pattern = pattern[1 + sizeof(val) + 1 + sizeof(ws)..];
        }

        if (!has_prefix(pattern, "]"))
          return;
        pattern = pattern[1..];
        break;

      default:
        return;
      }
    }

    data = new_data;
  }

  int(0..1) match_node(Parser.XML.Tree.SimpleNode n)
  {
    if (!data || !n || (n->get_node_type() != Parser.XML.Tree.XML_ELEMENT))
      return 0;

    //  Validate name
    string tag = n->get_tag_name();
    mapping attrs = n->get_attributes();
    if ((data->element != "*") && (data->element != tag))
      return 0;

    //  Validate ID
    if (data->id && (attrs->id != data->id))
      return 0;

    //  Validate classes
    if (sizeof(data->classes)) {
      string cls_norm = String.normalize_space(attrs->class || "");
      array(string) node_classes = (cls_norm / " ") - ({ "" });
      foreach ([array] data->classes, string cls) {
        if (!has_value(node_classes, cls))
          return 0;
      }
    }

    //  Validate attributes
    constant eval_fn = ([ "="  : `==,
                          "^=" : has_prefix,
                          "$=" : has_suffix,
                          "*=" : has_value ]);
    foreach ([array] data->attrs, array attr_rule) {
      [string match_name, string op, string match_val] = attr_rule;
      string val = attrs[match_name];
      if (!val)
        return 0;

      if (op) {
        if (function fn = eval_fn[op]) {
          if (!fn(val, match_val))
            return 0;
        } else if (op == "~=") {
          array(string) tokens = (String.normalize_space(val) / " ") - ({ "" });
          if (!has_value(tokens, match_val))
            return 0;
        } else if (op == "|=") {
          if ((val != match_val) && !has_prefix(val, match_val + "-"))
            return 0;
        }
      }
    }
    return 1;
  }
};


//! Checks if @[c] is not a control character. Exceptions are @tt{\n@},
//! @tt{\r@} and @tt{\t@}.
int(0..) is_normal_char(int c)
{
  if (c & ~0x9f) return 1;            // Not a control char.
  return (< '\n', '\r', '\t' >)[c];   // Is white-space.
}

//! Removes control chars. See also @[is_normal_char()].
string filter_control_chars(string in)
{
  return (string)filter((array(int))in, is_normal_char);
}
variant array(string) filter_control_chars(array(string) in)
{
  return map(in, filter_control_chars);
}



//!  Try to join sequences smartly so we cut down long lists. For example:
//!
//!    ({ "Stack", "62", "63", "64", "65", "66", "67" })  =>  "Stack, 62-67"
//!    ({ "14A", "15A", "16A", "17A" })                   =>  "14A-17A"
//!    ({ "14A", "15", "16", "17" })                      =>  "14A, 15-17"
//!
//!  We solve this by scanning and replacing subsequent numbers with
//!  a range entry directly in the array before joining. Elements that
//!  don't start with a numeric value are assumed to include a section
//!  prefix (which must be a sequence of letters only) which are matched
//!  on as well, or just be a non-groupable string like "Stack".
//!
//!  If a custom @[separator] is not provided the default is ", ".
//!
//!  Based on group_page_nums() in common.js
string group_page_nums(array(string) apps, void|string separator)
{
  if (!sizeof(apps))
    return "";
  if (sizeof(apps) == 1)
    return apps[0];

  Regexp split_app_parts_re = Regexp("^([a-zA-Z]*)([0-9]*)([^,-]*)");

  mapping app_parts(int idx)
  {
    string app_str = apps[idx];
    array parts = split_app_parts_re->split(app_str);
    if (parts && sizeof(parts) == 3) {
      if (sizeof(parts[1]))
        return ([ "section": parts[0],
                  "num": (int) (parts[1] || "0"),
                  "subspec": parts[2] ]);
    }
    return 0;
  };

  int range_start_idx = 0;
  mapping match_parts = app_parts(0);
  array(string) new_apps = ({ });
  for (int k = 1; k < sizeof(apps); k++) {
    //  Require non-empty page number and suffix match for valid range
    int is_last = (k == sizeof(apps) - 1);
    int range_end_idx = k - 1;
    mapping cur_parts = app_parts(k);
    if (match_parts && cur_parts &&
        (cur_parts->section == match_parts->section) &&
        (cur_parts->subspec == match_parts->subspec)) {
      //  Is the number part same as previous plus one? If so we keep
      //  extending the range unless we reached the end of the array.
      int steps = k - range_start_idx;
      if (cur_parts->num == match_parts->num + steps) {
        range_end_idx = k;
        if (!is_last)
          continue;
      }
    }

    //  End of an earlier range?
    if (range_end_idx - range_start_idx > 0) {
      string range_str = apps[range_start_idx] + "-" + apps[range_end_idx];
      new_apps += ({ range_str });
    } else {
      //  No range; just output the previous value
      new_apps += ({ apps[range_start_idx] });
    }

    //  If we're stopping here we may need to include the current entry if
    //  it's not already part of a previous range.
    if (is_last && (range_end_idx < k))
      new_apps += ({ apps[k] });

    //  Start of new range
    range_start_idx = k;
    match_parts = cur_parts;
  }
  return new_apps * (separator || ", ");
}


string get_forwarded_field(RequestID id, string field)
{
  //  Borrowed from roxen.pike
  foreach (id->misc->forwarded || ({ }), array(string|int) segment) {
    if (!arrayp(segment) || sizeof(segment) < 3) continue;
    if (segment[0] != field || segment[1] != '=') continue;
    return MIME.quote(segment[2..]);
  }
  return 0;
}


// Keep a reference to all in-flight queries -- otherwise they might get
// zapped by the Pike GC.
multiset(REP.StreamingHTTPQuery) queries_in_progress = (<>);

//! Wrapper for Protocols.HTTP.do_async_method that returns a
//! @[Concurrent.Future], with the @[Query] object as the successful
//! result. Follows redirects. Todo: make use of
//! Protocols.HTTP.Promise when we have Pike >= 8.1.
Concurrent.Future/*REP.StreamingHTTPQuery*/
do_async_method (string method,
                 string|Standards.URI uri,
                 void|mapping(string:int|string|array(string)) query_variables,
                 void|mapping(string:string|array(string)) request_headers,
                 void|string|Stdio.Stream request_data)
{
  Concurrent.Promise promise = Concurrent.Promise();

  void cleanup_query (Protocols.HTTP.Query q) {
    q->set_callbacks (0, 0);
    q->close();
    queries_in_progress[q] = 0;
  };

  void got_data(REP.StreamingHTTPQuery q) {
    if (q->status / 100 == 2) {
      //  For DELETE requests we return a true result
      if (upper_case(method) == "DELETE")
        promise->success(1);
      else
        promise->success(q);
    } else if (q->status == Protocols.HTTP.HTTP_NOT_MODIFIED) {
      //  Should only happen if the caller added If-None-Match. We return
      //  the query object here as well so the caller can inspect response
      //  headers.
      promise->success(q);
    } else if (q->status == Protocols.HTTP.HTTP_MOVED_PERM ||
               q->status == Protocols.HTTP.HTTP_FOUND) {
      if (string redirect = q->headers->location) {
        Standards.URI new_uri = Standards.URI (redirect, uri);
        Concurrent.Future f = do_async_method (method, new_uri, query_variables,
                                               request_headers, request_data);
        f->on_success (promise->success);
        f->on_failure (promise->failure);
        f = 0;
      } else {
        string msg = sprintf("Didn't get a Location header for redirect in %O: %O\n",
                             q, q->headers);
        promise->failure(HttpError(msg, q));
      }
    } else {
      string msg = sprintf("Unexpected status in %O (%s): %O\n",
                            q, uri, q->data());
      promise->failure(HttpError(msg, q));
    }

    cleanup_query (q);
    promise = 0;
    q = 0;
  };

  void transfer_failed(Protocols.HTTP.Query q) {
    string msg = sprintf("Connection error in %O (%s): %d\n",
                          q, uri, q->errno);
    promise->failure(HttpError(msg, q));
    cleanup_query (q);
    promise = 0;
  };

  REP.StreamingHTTPQuery q = REP.StreamingHTTPQuery();
  q->set_callbacks (lambda (REP.StreamingHTTPQuery q,
                            function got_data, function transfer_failed)
                    {
                      q->timed_async_fetch (got_data, transfer_failed);
                    }, transfer_failed, got_data, transfer_failed);

  string string_data;
  if (objectp (request_data))
    q->set_source_stream (request_data);
  else if (stringp (request_data))
    string_data = request_data;

  Protocols.HTTP.do_async_method (method, uri, query_variables, request_headers,
                                  q, string_data);
  queries_in_progress[q] = 1;
  q = 0;

  return promise->future();
}

class HttpError {
  inherit Error.Generic;

  constant is_http_error = true;
  Protocols.HTTP.Query q;

  void create(string message, Protocols.HTTP.Query q) {
    ::create(message);
    this::q = q;
  }

  int http_status() {
    return this::q->status;
  }

  mapping(string:string) http_headers() {
    return this::q->headers || ([]);
  }
}

mixed decode_http_response (REP.StreamingHTTPQuery q)
{
  if (!objectp(q))
    return q;

  if (q->status == Protocols.HTTP.HTTP_NOT_MODIFIED)
    return UNDEFINED;

  if (q->status / 100 == 2) {
    if (string ct_header = q->headers["content-type"]) {
      array(string) parts = map (ct_header / ";", String.trim_all_whites);
      string content_type = parts[0];
      string data = q->unicode_data();

      switch (content_type) {
      case "text/xml":
      case "application/xml":
        return Parser.XML.Tree.simple_parse_input (data);
      case "application/json":
        return Standards.JSON.decode (data);
      default:
        return data;
      }
    }
  }

  error ("Unsuccessful decode of %O: %O.", q, q->data()[..500]);
}

//! Like @[do_async_method], but decodes data according to the
//! returned Content-Type header. Succeeds with a
//! @[Parser.XML.Tree.SimpleRootNode] or a json-decoded structure. If the
//! Content-Type header specifies a charset, data is charset decoded
//! first.
Concurrent.Future/*Parser.XML.Tree.SimpleRootNode|mixed*/
do_async_method_decode
  (string method,
   string|Standards.URI uri,
   void|mapping(string:int|string|array(string)) query_variables,
   void|mapping(string:string|array(string)) request_headers,
   void|string|Stdio.Stream request_data)
{
  return do_async_method (method, uri, query_variables, request_headers,
                          request_data)->map (decode_http_response);
}

//! Preauthenticates a URI by calling acauth_cookie's preauth_uri method.
//! The provided @[uri] argument is modified destructively and returned.
//! On errors, such as that @[ac_handle] doesn't exist, the error is reported
//! and the original @[uri] is returned.
Standards.URI preauth_uri (Standards.URI uri,
  Configuration conf,
  string ac_handle)
{
  if (!sizeof (ac_handle))
    return uri;

  RoxenModule cookie_mod = conf->find_module ("acauth_cookie");
  if (!cookie_mod) {
    report_error ("REP.Utils.preauth_uri: No acauth_cookie module loaded.\n");
    return uri;
  }

  REP.REPSession ses = REP.get_session();
  AC.ModuleAC mac = ses->mac;
  AC.AC_DB acdb = mac->get_db()();
  AC.Identity acid = acdb->identities->find_by_handle(ac_handle, 0);

  if (!acid) {
    report_error ("REP.Utils.preauth_uri: User %O doesn't exist.\n",
      ac_handle);
    report_error (describe_backtrace(backtrace()));
    return uri;
  }

  return cookie_mod->preauth_uri(uri, acid);
}


//!  Generates an SVG string that represents one or more rectangles. The
//!  units in the output string is converted to points. This is typically
//!  used in various e-paper export routines, e.g. Visiolink and Print
//!  Automation.
//!
//!  @param bounds_mm_list
//!    Array of individual rectangles where each is specified as a mapping
//!    with keys @[left], @[top], @[width] and @[height], all expressed in
//!    in millimeters from top-left corner of the page.
//!
//!  @returns
//!    Returns a SVG string that represents the bounds rectangles converted
//!    to points.
string get_svg_for_bounds(array(mapping(string:int|float)) bounds_mm_list)
{
  ASSERT_IF_DEBUG(sizeof(bounds_mm_list));

  //  All coordinates must be given in pt and (0, 0) is in top-left corner
  //  of page. Fill, stroke and width are optional so we leave them out.
  //  Multiple rectangles can be listed to more precisely describe an article
  //  area.

  //  Convert mm to points. We expect to find "top", "left", "width" and
  //  "height" keys in the bounds mapping.
  //
  //  Note that we might get negative coordinates or bounds outside the page
  //  for an article that e.g. spans a spread.
  constant PT_PER_MM = 72.00 / 25.4;  //  72.00 pt per inch
  array(mapping(string:float)) bounds_pt_list =
    map(bounds_mm_list,
        lambda(mapping(string:int|float) bounds_mm) {
          ASSERT_IF_DEBUG(sizeof(bounds_mm &
                                 ({ "left", "top", "width", "height" }) ) == 4);
          return ([ "left"   : (float) bounds_mm->left   * PT_PER_MM,
                    "top"    : (float) bounds_mm->top    * PT_PER_MM,
                    "width"  : (float) bounds_mm->width  * PT_PER_MM,
                    "height" : (float) bounds_mm->height * PT_PER_MM ]);
        });

  return
    "<svg xmlns='http://www.w3.org/2000/svg'"
    " xmlns:xlink='http://www.w3.org/1999/xlink'>\n" +
    map(bounds_pt_list,
        lambda(mapping(string:float) bounds_pt) {
          //  Defining the page boundaries requires an id="page" attribute,
          //  and for articles the id should be left unset.
          return
            sprintf("<rect x='%.1fpt' y='%.1fpt' width='%.1fpt' height='%.1fpt'/>\n",
                    bounds_pt->left,
                    bounds_pt->top,
                    bounds_pt->width,
                    bounds_pt->height);
        }) * "" +
    "</svg>";
}
