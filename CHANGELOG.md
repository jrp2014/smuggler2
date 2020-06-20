# Changelog

Notable changes to `Smuggler2`

`smuggler2` uses [PVP Versioning][1].
The change log is available [on GitHub][2].

##  Unreleased
- print out timings / memory usage, when `-v2` or greater is set

##  [0.3.6.1]: --  19 June 2020
- improve handling of pattern synonyms
- remove ghc-smuggler2 from build. Provide a scipt instead, to appease cabal

##  [0.3.5.2]: --  15 June 2020
- tweaks to allow `smuggler2` to run under Windows

##  [0.3.5.1]: --  14 June 2020
- fix bug that left some files with open imports unprocessed
- add `LeaveOpenImports` and `MakeOpenImports` options

##  [0.3.4.2]: --  10 June 2020
- Add test golden files to the distribution

##  [0.3.4.1]: --  9 June 2020
- Place output from CPP and literate Haskell sources into new files

##  [0.3.3.2]: --  8 June 2020
- pattern synonyms and type families are handled
- ghc-smuggler2 wrapper for ghc

##  [0.3.2.2]: --  31 May 2020
- Documentation only

##  [0.3.2.1]: --  30 May 2020
- Provisional version for Hackage


[1]: https://pvp.haskell.org
[2]: https://github.com/jrp2014/smuggler2/releases
