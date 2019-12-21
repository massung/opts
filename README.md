# The OPTS Package

The `opts` package is a small, simple, command line options parser for Common Lisp that is implementation-independent.

## Quickstart

There are only three functions exposed by the `opts` package:

(**parse-opts** *args opts*)

(**next-arg** ***&key*** *read*)

(**write-help** *opts* ***&optional*** *stream*)

Typical usage would be to construct a global list that are the command line parameters for your app. Each command has the following format:

```lisp
(name flags &key action help &allow-other-keys)
```

The `name` is a value (typically a symbol) that will be used as the key in an associative list returned by `parse-opts`.

The `flags` are a list of allowed matches for `name`. For example: `'("-v" "--verbose")`.

The `help` parameter is an optional string that is used with `write-help` to output what each parameter does.

The `action` is a function to call when a matching flag has been parsed. An `action` should only take keyword arguments and the return value will be the value of `name` in the resulting associative list. If `nil` (the default), then no function will be called and the value `T` will be implicit. This is useful for simple flag setting.

The `&allow-other-keys` is there so that additional, constant parameters can be passed to the `action` function.

## Parsing Arguments

Here is a simple example of defining an options list and parsing a list of command line arguments with it.

```common lisp
(defparameter *opts*
  '((n    ("-n")          :help "countdown from n" :action next-arg :read t)
    (by-2 ("-b" "--by-2") :help "countdown by 2")))
```

It's now possible to parse command line arguments with it:

```commonlisp
CL-USER> (parse-opts '("-n" "10" "--by-2" "file.txt") *opts*)
((N 10) (BY-2 T))
("file.txt")
```

Parsing stops at the first unrecognized argument, and the return values of `parse-opts` are the associative list of options that were successfully parsed along with any remaining, unparsed arguments.

## Capturing Arguments

You've already seen an example above of argument capturing (with `-n`). This is done with the `next-arg` function.

While inside the execution of `parse-opts`, the "next" argument that would be parsed is able to be captured with `next-arg`. The `next-arg` function takes a single, optional, keyword parameter: `read`. If `read` is `nil` (the default), then the next argument is returned verbatim (as a string) or is `nil` if there are no more arguments left. If `read` is `T`, then the Common Lisp `read` function is used to parse the argument with `*read-eval*` set to `nil`. Otherwise, it is assumed that `read` is a symbol or function and will be called with the next argument.

Finally, as long as all your positional arguments are last, you can use the second return value of `parse-opts` as a way to capture the remaining arguments.

## Help Text

Finally, `write-help` can be used to output a simple help string to a stream:

```commonlisp
CL-USER> (write-help *opts*)
  -n                      countdown from n
  -b  --by-2              countdown by 2
```

Notice that this output outputs the command line flags and help text; there is no attempt to print any detailed help or usage.

# fin.

If you get some good use out of this package, please let me know; it's nice to know your work is valued by others.

Should you find/fix a bug or add a nice feature, please feel free to send a pull request or let me know at [massung@gmail.com](mailto:massung@gmail.com).
