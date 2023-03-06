# camel2snake

Tool to convert identifiers in Zig code from camel case to snake case.
Operates on .zig files and directory trees with .zig files.

## Quick start

```shell
$ zig build
```

List files that would change without modifying them:

```shell
$ ./zig-out/bin/camel2snake --dry-run ~/zig/lib/std # no changes by default
$ ./zig-out/bin/camel2snake --dry-run ~/zig/lib/std --convert=maxInt
$ ./zig-out/bin/camel2snake --dry-run ~/zig/lib/std --convert-all --except=maxInt
```

Print contents of Zig source files with proposed changes, coloring affected identifiers:

```shell
$ ./zig-out/bin/camel2snake --dry-run-highlight ~/zig/src/Sema.zig --convert-all | less -R
$ ./zig-out/bin/camel2snake --dry-run-highlight ~/zig/lib/std/math/big \
  --convert=maxInt --convert=initSet=init_spicy | less -R
```

Convert all camel case identifiers, modifying files in place:

```shell
$ ./zig-out/bin/camel2snake my/file.zig ../other/main.zig \
  --convert-all --adult-camels \
  --except=oneFunc --except=TwoType \
  --load-rules=custom_changes.txt \
  --load-rules=some_c_library_identifiers.txt
```

Token matching and replacement rules, as well as wildcard patterns,
can be specified in text files and applied with `--load-rules`.
See `--help-rules` for more information.

## Notes

Any token that is not detected to be camel case is always ignored.

This tool does not use a real Zig tokenizer or any parser,
and will 100% affect words in comments and string literals,
in addition to any Zig builtin functions and non-functions.
See the [function style change proposal][1] for context.

[1]: https://github.com/ziglang/zig/issues/1097#issuecomment-1405101822
