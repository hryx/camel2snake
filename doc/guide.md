# Convert camel case to snake case

## Quick overview of the tool

TODO

## Process for updating the Zig compiler sources

These steps assume you are in `<path to zig>/build` and have built a stage3 Zig following
the [directions on the Wiki](https://github.com/ziglang/zig/wiki/Contributing#editing-source-code).

You need [camel2snake](https://github.com/hryx/camel2snake) installed somewhere.
If you would like to see information about changes at any step,
just add the `--stats` to the camel2snake command.

1. Zig must be updated to recognize builtins with their soon-to-change names.
  Edit the ComptimeStringMap in `src/BuiltinFn.zig`. There is also one _call_ to a builtin in
	this file (`@setEvalBranchQuota()`), which we can't change yet; you can either edit this file
	by hand, or run the tool and just fix this one call after.

```shell
camel2snake ../src/BuiltinFn.zig --convert-builtins
sed -r -i '' 's~([^"]@)set_eval_branch_quota~\1setEvalBranchQuota~g' ../src/BuiltinFn.zig
```

2. Build stage4 which recognizes renamed builtins:

```shell
stage3/bin/zig build -p stage4 -Dno-lib -Denable-llvm
```

3. Convert all builtin names in compiler Zig sources.
  This also updates src/translate_c.zig to emit `"@compile_error"`
	instead of `"@compileError"` for macros, which is now necessary to make C imports work.
  `--fixup-compile-error-tests` applies post-processing that changes the expected
  line and column numbers in the expected output of test/cases/compile_errors tests.
	`--max-file-kb=` bumps up the file size limit because one test file is >10MB.

```shell
camel2snake ../src ../lib ../build.zig ../test ../tools \
	--max-file-kb=12000 \
	--convert-builtins \
	--fixup-compile-error-tests \
	--ignore-path=zig-cache
```

4. With all builtin names changed, build stage5 and run tests.
  (Note: I had had to clear some zig-cache/o/*/cimport.zig files with old `@compileError`s.
  I don't know the cache system very well, so there is certainly an explanation and a
  more appropriate way to avoid this.)

```shell
stage4/bin/zig build -p stage5 -Dno-lib -Denable-llvm
stage5/bin/zig build test-std -Dskip-release -Dskip-non-native
stage5/bin/zig build test-cases -Dskip-release -Dskip-non-native
stage5/bin/zig build test-behavior -Dskip-release -Dskip-non-native
```

5. Before converting the docs, there are a couple of `#link`s in langref.html.in
  that need to be fixed. That don't have `@` before the builtin names and need
  to be normalized, otherwise the links are wrong after conversion.
  Then the docs can be converted and built/tested.

```shell
sed -r -i '' 's~(\#link\|)([a-z]+[A-Z][^}]+\})~\1@\2~g' ../doc/langref.html.in
camel2snake ../doc --convert-builtins --allow-non-zig-files
stage5/bin/zig build docs -Dskip-release -Dskip-non-native
```

6. TODO
99. Update stage1 wasm blob.

```shell
stage5/bin/zig build update-zig1
```

## Tips for third-party Zig projects

TODO

## About adult camels

TODO
