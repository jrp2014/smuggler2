# Things to be done

- [X] Does CPP work? Yes, but the mark is dropped, so put output into a new
  file.  Similarly for literate Haskell.

- [x] Script to generate new test (from template?) Not worth the effort

- [X] get tasty-golden to delete successful results and update tests where
      smuggler makes no changes

- [ ] refactor the much more sophisticated https://github.com/ddssff/refact-global-hse

- [ ] Do a better job of preserving comments

- [X] Running test suite on both an imported module and the module that imports
      it creates race conditions because both cases will generate the same minimum
      imprts dump file

- [X] Do a better job of preserving imported pattern annotation.

- [X] Check that type imports / exports work, Type Operators,

- [X] Running test suite on both an imported module and the module that imports
      it creates race conditions because both cases will generate the same minimum
      imports dump file

- [x] User ghc environment files instead of `cabal exec` to launch tests.  But
  these are invisible and so likely to trip you up

- [ ] Use NamedFieldPuns / RecordWildCards for Options

- [ ] Check that an export does not need to be qualified

- [ ] Figure out why github workflow uses ghc 8.10.1 when it should be running 8.8.3

- [X] Add options for handling `NoImplictPrelude` and keeping it pristine?

- [ ] Add option to import using widcards only

- [ ] Rexporting of patterns?

- [X] Initial language pragmas are not retained if there is no `module ...
  where`

- [ ] `ghc-exactprint` adds `\r` into comments at eol. Woraround?

- [ ] check `getMinimalImports` for corner cases
