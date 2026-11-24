# Development

## Building and testing

With cabal (as CI does):

```sh
cabal build
cabal test
```

In the TokTok monorepo, with Bazel:

```sh
bazel build //hs-happy-arbitrary/...
bazel test //hs-happy-arbitrary/...
```

To run some of the tests, pass hspec's `--match`, for example
`--test_arg=--match=Mini` with Bazel or `--test-options=--match=Mini` with
cabal.

## Tests

- `test/Language/`: tests for each module.
- `test/Examples/Mini/`: a small language with its own Happy parser and Alex
  lexer. `Examples.MiniSpec` checks that every generated program is one its
  parser accepts.
- `test/examples/Cimple/`: a copy of Cimple's lexer and grammar, a real
  language with many start codes. Keep it in sync with hs-cimple.

## Trying a grammar

`tools/generate.hs` (Bazel only) prints random programs and which rules they
used:

```sh
bazel run //hs-happy-arbitrary/tools:generate -- \
    --alex Lexer.x --happy Parser.y --start TranslationUnit --size 50 --iterations 10
```

Without `--happy`, it prints sample text for each token class of the lexer.
It joins tokens with spaces without asking a lexer, so a program can lex
differently from the tokens it was made of.
