# Design

happy-arbitrary makes random programs for a language whose parser is written
with Happy and Alex. It works from the parser's own files, so the programs
follow the real grammar, and it uses the language's lexer to check their text.

## Reading the grammar and the lexer

`Language.Happy.Parser` reads a Happy `.y` file. `Language.Happy.Grammar`
turns its rules into `Rules`: the productions of every nonterminal that the
start symbol reaches, with parameterised rules like `List(p)` instantiated for
every argument they're used with, so `List(Identifier)` is a nonterminal of its
own.

`Language.Alex.Parser` reads an Alex `.x` file: macros, character sets (with
`#` and `~`), regexes, start codes, blocks of rules for a start code, and the
code around them. Macros are expanded while parsing, so every regex stands on
its own. Left and right contexts are read but not kept.

## Linking tokens to lexer rules

Happy and Alex only meet in Haskell code: a `%token` pattern like
`{ L _ IdVar _ }` matches what a lexer action like `{ mkL IdVar }` makes. A
`LinkConfig` finds the token class in both (by default, the word after `L _`
and after `mkL`).

A lexer rule only applies in its start codes. To make a token in the current
start code, the linker looks for a rule that makes it there, or for a chain of
rules that only switch start code (like `begin comment`) to get to one. It
follows `andBegin` and `begin` to know the start code after each token.

## Trees of a given size

`Language.Happy.Sized` draws a tree with QuickCheck's size in tokens:

- Every nonterminal knows the fewest and the most tokens it can make. Recursive
  rules have no most.
- A node picks one of the productions that can make its budget. The more
  symbols a production reaches, the more likely it is, by the square root, so
  that small productions still come up.
- The budget is shared among the children in random order, so that each child
  can still make its share. Children that reach more symbols get more of the
  spare tokens, so a list of one token doesn't take up most of the tree.

Trees come out at the size asked for, or as close as the grammar allows.

We tried Boltzmann sampling first. It draws trees uniformly at a given size, so
for a real grammar most of the size goes into whichever construct has the most
trees. For Cimple's grammar, the expected size also stayed below 10 tokens
however close we got to the singularity.

## Text for the tokens

`Language.Alex.Arbitrary` makes text that a regex matches, with at most 5
repetitions. A regex like `[a-z]+` can make another class's keyword, which
would lex as that class, so the linker draws again when it does.

The text of one token can still lex differently next to its neighbours: `=`
and `=` without a space become `==`, and where the lexer treats spaces and
newlines as tokens (comments, preprocessor lines), a space between tokens is an
extra token. `generateChecked` takes the language's lexer and draws a token's
text, and the space before it (nothing, a space or a newline), again until the
lexer finds exactly the tokens generated so far.

A grammar can allow a token where its lexer can't make it, like a comment in a
preprocessor line that the lexer only reads at the start of a line. If a token
keeps failing, `generateChecked` draws a new tree, up to 100 times.
`renderChecked` says which token and start code failed.

Without a lexer, `generateSource` joins the tokens' text with spaces.

## What the test has to do

The grammar doesn't have the checks in the parser's actions, or in later passes
over the tree, so some generated programs are rejected by the real parser. The
test drops those, for example with `suchThatMap`.
