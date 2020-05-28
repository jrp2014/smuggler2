# Things to be done

- [X] cut out unused code

- [X] reuse mkParens and other helpers from retrie

- [X] refactor so that ast and anns are not threaded so explicitly through the code

- [x] check that source is not rewritten if there are no changes (works only in
  some cases)

- [ ] testing: check that smuggler really works with different export types

- [ ] does hiding work?

- [ ] does CPP work? (should do)

- [ ] Script to generate new test (from template?)

- [X] get the tests to work by generating correct golden values for different
  combinations of options.

- [ ] get tasty-golden to delete successful results and update tests where
  smuggler makes no changes

- [ ] try using some of the retrie apparatus

- [ ] refactor the much more sophisticated https://github.com/ddssff/refact-global-hse

- [ ] remove / replace the orignial Data.Bool - based tests, which produce
  different results for ghc 8.6.5

- [X ] Show a banner when the plug-in is invoked

- [ ] Do a better job of preserving comments

- [X ] Do a better job of rendering operators for export, or at least exclude
  them

- [X] Stop smuggler2 errors being reported as GHC panics
