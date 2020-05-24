# smuggler

![smuggler-logo](https://user-images.githubusercontent.com/4276606/45937457-c2715c00-bff2-11e8-9766-f91051d36ffe.png)

<!--
[![Hackage](https://img.shields.io/hackage/v/smuggler.svg?logo=haskell)](https://hackage.haskell.org/package/smuggler)
[![Build](https://img.shields.io/travis/kowainik/smuggler.svg?logo=travis)](http://travis-ci.org/kowainik/smuggler)
-->

[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/smuggler/blob/master/LICENSE)
![Github CI](https://github.com/jrp2014/smuggler/workflows/Smuggler/badge.svg)

> “So many people consider their work a daily punishment. Whereas I love my work
> as a translator. Translation is a journey over a sea from one shore to the
> other. Sometimes I think of myself as a smuggler: I cross the frontier of
> language with my booty of words, ideas, images, and metaphors.”
>
> ― Amara Lakhous, Clash of Civilizations Over an Elevator in Piazza Vittorio

Haskell Source Plugin which removes unused imports and adds explicit exports automatically.

## How to use

Add `smuggler` to the dependencies of your project. Then add the following
compiler options to your build configuration (eg, to `ghc-options` in your
`.cabal` file):

```
-fplugin=Smuggler.Plugin
```

The Plugin has serveral (case-insensitive) options:

- `NoImportProcessing` - do no import processing
- `PreserveInstanceImports` - remove unused imports, but preserve a library import stub.
  such as `import Mod ()`, to import only instances of typeclasses from it. (The default.)
- `MinimiseImports` - remove unused imports, including any that may be needed to
  import typeclass instances. This may, therefore, stop the module from compiling.

- `NoExportProcessing` - do no export processing
- `AddExplicitExports` - add an explicit list of all available exports (excluding
  those that are imported) if there is no existing export list. (The default.)
  You may want to edit it to keep specific values, types or classes local to the module.
  At present, a typeclass and its class methods are exported individually. You may want to
  replace those exports with an abbreviation such as `C(..)`.
- `ReplaceExports` - replace any existing module export list with one containing all
  available exports (which, again, you can, of course, then prune to your requirements).

Any other option value is used to generate a source file with the option value used as
a new extension rather than replacing the original file. For example,

```
-fplugins-opt=Smuggler.Plugin:new
```

will create output files with a `.new` suffix rather the overwriting the originals.

A lovely addition to this package is that it automatically supports on-the-fly
feature if you use it with `ghcid`. Smuggler doesn't perform file changes when
there are no unused imports. So you can just run `ghcid` as usual:

```
ghcid --command='cabal repl'
```

## Caveats

- By default `smuggler` does not remove imports completely because an import may be being
  used to only import instances of typeclasses, So it will leave stubs like

  ```haskell
  import Mod ()
  ```

  that you may need to remove manually. Alternatively use the `MinimiseImports` option to
  remove them anyway.

- Any comments in the import block will be discarded. Similarly, blank lines in
  in the import section will be discarded.

- CPP files may not be processed correctly (fixed?)

- `smuggler` depends on the current `ghc` compiler and `base` library to check
  whether an import is redundant. Earlier versions of the compiler may, of
  course, need it. The [base library
  changelog](https://hackage.haskell.org/package/base/changelog) provides some
  details of what was made available when.

- Literate Haskell `lhs` files are not supported

- `hiding` clauses may not be properly analysed

## For contributors

Requirements:

- `ghc-8.6.5`, `ghc-8.8.3` and `ghc-8.10.1` Smuggler is untested with earlier versions.
  and some of the tests fail on `ghc-8.6.5` because it needs to import `Data.Bool` whereas
  later versions of GHC don't.
- `cabal >= 3.0` (ideally `3.2`)

### Cabal: How to build?

```shell
$ cabal update
$ cabal build
```

To build with debugging:

```shell
$ cabal bulid -fdebug
```

Curently this just adds an `-fdump-minimal-imports` parameter to GHC
compilation.

#### Stack: How to build?

```shell
$ stack build
```

### Run tests

There is a `tasty-golden`-based test suite that can be run by

```shell
$ cabal test smuggler-test --enable-tests
```

Further help can be found by

```shell
$ cabal run smuggler-test -- --help
```

(note the extra `--`)

For example, if you are running on `ghc-8.6.5` you can

```shell
$ cabal run smuggler-test -- --accept
 ```
to update the golden outputs to the current results of (failing) tests.

It is sometimes necessary to run `cabal clean` before running tests to ensure
that old artefacts do not lead to misleading results.

`smuggler-test` uses `cabal exec ghc` internally to run a test.  The `cabal` command
that is to be used to do that can be set using the `CABAL` environment variable.
This may be helpful for certain workflows where `cabal` is not in the current
path, or you want to add extra flags to the `cabal` command.

## Implementation approach

`smuggler` uses the `ghc-exactprint`
[library](https://hackage.haskell.org/package/ghc-exactprint) to modiify the
source code. The documentation for the library is fairly spartan, and the
library is not widely used, so the use here can, no doubt, be optimised.

The library is needed because the annotated AST that GHC generates does not have enough
information to reconstitute the original source.
Some parts of the renamed syntax tree (for example, imports) are
not found in the typechecked one. `ghc-exactprint` provides parsers that
preserve this information, which is stored in a separate
`Anns` `Map` used to generate properly formatted source text.

To make manipulation of GHC's AST and `ghc-exactprint`'s `Anns` easier,
`ghc-exactprint` provides a set of Transform functions. These are intended to facilitate
making changes to the AST and adjusting the `Anns` to suit the changes.

> These functions are [said to be under heavy development](https://hackage.haskell.org/package/ghc-exactprint-0.6.3/docs/Language-Haskell-GHC-ExactPrint-Transform.html).
> It is not entirely obvious how they are intended to be used or composed. The
> approach provided by [`retrie`](https://hackage.haskell.org/package/retrie)
> wraps an AST and `Anns` into a single type that seems to make AST
> transformations easier to compose and reduces the risk of the `Anns` and AST getting
> out of sync as it is being transformed, something with which the type system doesn't
> help you since the `Anns` are stored as a `Map`.

### Imports

`smuggler` uses GHC to generate a set of minimal imports. It

- parses the original file
- dumps the minimal exports that GHC generates and parses them back in (to pick
  up the annotations needed for printing)
- drops implicit imports (such as Prelude) and, optionally, imports that are
  for instances only
- replaces the original imports with minimal ones
- `exactprint`s the result back over the original file (or one with a different
  suffix, if that was specified as option to `smuggler`)

A point of additional complexity is that the AST provided by GHC to `smuggler`
is of a different type from the AST that `ghc-exactprint` produces. (It is the
product of the renaming phase of the compiler, while `ghc-exactprint` produces
a parse phase AST.)

### Exports

Exports are simpler to deal with. GHC generates a list of all the things that
are in scope (`AvailInfo`) which can be used to generate a list of `Name`s
(currently via `availNamesWithSelectors`). This list is turned into Haskell
syntax used to replace the existing export list, if any.

## Other projects

- the original version of [`smuggler`](https://hackage.haskell.org/package/smuggler) on which this one is based
- `retrie` a [code modding tool](https://hackage.haskell.org/package/retrie)
  that works with GHC 8.10.1
- `refact-global-hse` an ambitious [import refactoring tool](https://github.com/ddssff/refact-global-hse).
  This uses `haskell-src-exts` rather than `ghc-exactprint` and so may not work with current versions of GHC.
- These blog posts contain some fragments on the topic of using `ghc-exactprint` to manipulate import lists
  [Terser import declarations](https://www.machinesung.com/scribbles/terser-import-declarations.html) and
  [GHC API](https://www.machinesung.com/scribbles/ghc-api.html) (The site
  doesn't always seem to be up.)
