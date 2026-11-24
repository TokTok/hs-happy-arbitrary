# Haskell package: happy-arbitrary

Random programs for testing parsers made with Happy and Alex. It reads the
parser's own grammar (`.y`) and lexer (`.x`), so the programs follow the
grammar, and it asks the language's lexer whether their text lexes back to the
tokens it generated.

## Example

A property that printing a parsed program and parsing it again gives the same
output:

```haskell
import Language.Alex.Parser (parseAlex)
import Language.Happy (parseGrammar, runAlex)
import Language.Happy.Linker (Lexer, defaultLinkConfig, generateChecked)

-- | The token classes our lexer finds, named like in the %token patterns.
classes :: Lexer
classes = either (const Nothing) (Just . map (Text.pack . show . MyLexer.lexemeClass)) . MyLexer.scan

spec :: Spec
spec = do
    alex <- runIO $ either error id . parseAlex <$> Text.readFile "src/Lexer.x"
    happy <- runIO $ either error id . flip runAlex parseGrammar <$> LBS.readFile "src/Parser.y"
    let programs = generateChecked defaultLinkConfig alex classes "Program" happy

    it "prints the same after parsing what it printed" $
        forAll (programs `suchThatMap` (either (const Nothing) Just . MyParser.parse)) $ \ast ->
            let printed = MyPrinter.print ast
            in MyPrinter.print <$> MyParser.parse printed `shouldBe` Right printed
```

Programs have QuickCheck's size in tokens. The grammar doesn't know the checks
in the parser's actions or in later passes, so the test drops the programs that
the real parser rejects.

By default, a `%token` pattern like `{ L _ IdVar _ }` names the token class
`IdVar`, and so does a lexer action like `{ mkL IdVar }`. A `LinkConfig` can
find the class in other code.

## Documentation

See [doc/design.md](doc/design.md) for how it works, and
[doc/development.md](doc/development.md) for building and testing it.
