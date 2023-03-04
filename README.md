# camel2snake

Tool to convert identifiers in Zig code from camel case to snake case.
Operates on .zig files and directory trees with .zig files.

## Quick start

```shell
$ zig build
```

List files that would change without modifying them:

```shell
$ ./zig-out/bin/camel2snake --dry-run ~/zig/lib/std
```

Print contents of Zig source files with proposed changes, coloring affected identifiers:

```shell
$ ./zig-out/bin/camel2snake --dry-run-highlight ~/zig/src/Sema.zig | less -R
```

By default, no identifiers are marked for change.
Use the `convert` options to make changes.

Convert all camel case identifiers, modifying files in place, with exceptions:

```shell
$ ./zig-out/bin/camel2snake --convert-all my/file.zig ../other/main.zig \
  --except=oneFunc --except=twoFunc
```

Use a list of files to convert or ignore certain identifiers:

```shell
$ cat preserve.txt
oneFunc
twoFunc
preciousAboutThisFunc
cantLiveWithoutCamel
$ cat fix.txt
prefersReptiles
livesInTheDesert
existentialDread
$ ./zig-out/bin/camel2snake --convert-all --except-list=preserve.txt special_case.zig
$ ./zig-out/bin/camel2snake --convert-list=fix.txt src/some_lib
```

## Notes

This tool does not use a real Zig tokenizer or any parser,
and will 100% affect words in comments and string literals,
in addition to any Zig builtin functions and non-functions.
See the [function style change proposal][1] for context.

[1]: https://github.com/ziglang/zig/issues/1097#issuecomment-1405101822
