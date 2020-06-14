# smuggler2

[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/jrp2014/smuggler2/blob/master/LICENSE)
![Smuggler2](https://github.com/jrp2014/smuggler2/workflows/Smuggler2/badge.svg)
[![Build Status](https://travis-ci.com/jrp2014/smuggler2.svg?branch=master)](https://travis-ci.com/jrp2014/smuggler2)
[![Hackage](https://img.shields.io/hackage/v/smuggler2.svg?logo=haskell)](https://hackage.haskell.org/package/smuggler2)
[![Stackage](https://www.stackage.org/package/smuggler2/badge/nightly?label=stackage)](https://www.stackage.org/package/smuggler2)

Smuggler2 is a Haskell GHC Source Plugin that automatically

- rewrites module imports to produce a minimal set. This may make code easier to
  read because the provenance of imported names is explcit.

- adds or replaces explicit exports to produce a maximalist set for hand
  pruning. All values, types and classes defined in a module are exported
  (excluding those that are imported). It does not check whether an exported
  name is used elsewhere in your package. Limiting exports may make it easier
  for `ghc` to optimise some code.

The [Haskell Wiki](https://wiki.haskell.org/Import_modules_properly) sets out
the pros and cons of using explicit import lists. `Smuggler2` offers the option
of leaving a module imports open (by not specifiying explcitly what is to be
imported from them) while developing and then getting `Smuggler2` to add minimal
lists of explicit exports. This helps to document modules and, arguably, makes
them easier to read by avoiding the need to qualify names to give an indication
of where they came from. It could also provides a cross-check that only expected
names are being used.

## How to use

Install `smuggler2` using `cabal install --lib smuggler2`.

If you also want the `ghc` wrapper, install it using
`cabal install exe:smuggler2`.

### Adding Smuggler2 to your dependencies

Add `smuggler2` to the dependencies of your project and to your compiler flags.
For example, you could include in your project `cabal` file something like

```Cabal
flag smuggler2
  description: Rewrite sources to cleanup imports, and create explicit exports
  exports
  default:     False
  manual:      True

common smuggler-options
  if flag(smuggler2)
    ghc-options: -fplugin=Smuggler2.Plugin
    build-depends: smuggler2 >= 0.3 && < 0.4
```

and then `import: smuggler-options` in the appropriate `library` or `executable`
sections.

The use of the flag allows you to build with or without source processing. Eg,

```bash
$ cabal build -fsmuggler2
```

using the example above.

You might use this approach to refine your imports or get a starting point for
your exports, but not rewrite them every time you compile. The use of a flag
means that you can also exclude `smuggler2` dependencies from your final builds.

### Alternatively, using a local version

If you have installed `smuggler2` from a local copy of this repository, you may
need to add `-package smuggler2` to your `ghc-options` if you did not install
using the `--lib` flag to `cabal install`.

### Or use a `ghc` wrapper

The `smuggler2` package provides an executable `ghc-smuggler2` that calls `ghc`
with the `-fplugin=Smuggler2.Plugin` argument (followed by any others that you
supply). This allows you to run the plugin over your sources without modifying
your `.cabal` file:

```bash
$ cabal build -with-compiler=ghc-smuggler2
```

or just

```bash
$ cabal build -w ghc-smuggler2
```

Smuggler2 tries not to change files when there is no work to do.

You can just run `ghcid` as usual:

```bash
$ ghcid --command='cabal repl'
```

## Options

`Smuggler2` has several (case-insensitive) options, which can be set by adding
`-fplugin-opt=Smuggler2.Plugin:` flags to your `ghc-options`

- `NoImportProcessing` - do no import processing
- `PreserveInstanceImports` - remove unused imports, but preserve a library
  import stub. such as `import Mod ()`, to import only instances of typeclasses
  from it. (The default.)
- `MinimiseImports` - remove unused imports, including any that may be needed
  only to import typeclass instances. This may, therefore, stop the module from
  compiling.

- `NoExportProcessing` - do no export processing
- `AddExplicitExports` - add an explicit list of all available exports
  (excluding those that are imported) if there is no existing export list. (The
  default.) You may want to edit it to keep specific values, types or classes
  local to the module. At present, a typeclass and its class methods are
  exported individually. You may want to replace those exports with an
  abbreviation such as `C(..)`.
- `ReplaceExports` - replace any existing module export list with one containing
  all available exports (which, again, you can, of course, then prune to your
  requirements).

- `LeaveOpenImports` and `MakeOpenImports` take a comma-separated list of module
  names. The specified modules are to be left open if they were open in the
  sourcee (in the case of `LeaveOpenImports`) and made open even if they were
  not originall (in the case of `MakeOpenImports`). For example, you could add

  ```bash
  -fplugin-opt=Smuggler2.Plugin:LeaveOpenImports:Relude,RIO,Prelude,Some.Module
  ```

  This may be helpful if you use ghc's `NoImplicitPrelude` language feature and
  import a prelude manually.

  If the `PreserveInstanceImports` option was sepecified, the `LeaveOpenImports`
  and `MakeOpenImports` options override it for the specified modules, They have
  no effect, if `NoImportProcessing` was specified. If a module is specified
  both to be left open and made open, it will be made open.

- Any other option value is used to generate a source file with a new extension
  of the option value (`new` in the following example) rather than replacing the
  original file.

  ```Cabal
  ghc-options: -fplugin=Smuggler2.Plugin -fplugin-opt=Smuggler2.Plugin:new
  ```

  This will create output files with a `.new` suffix rather the overwriting the
  originals.

## Caveats

Because `cabal` and `ghc` don't have full support for distinguishing dependent
packages from plug-ins you will probably want to ensure that the build the
dependencies for your project tha are installed into your local package db
first, before enabling sumuggler, otherwise they will all be processed by it
too, as your project builds, which should do no harm, but will increase your
build time.

`Smuggler2` is robust -- it can chew through the
[Agda](https://github.com/agda/agda) codebase of over 370 modules with complex
interdependencies and be tripped over by only

- a couple of ambiguous exports (are we trying to export something defined in
  the current module or something with the same name from an imported module)
- and a couple of imports where both qualifed and unqualifed version of the
  module are imported and there are references to both qualified and unqualifed
  version of the same names
- some qualified record fields are overlooked

But there are some caveats, most of which are either easy enough to work around
(and still offer the benefit of a great reduction in keyboard work):

- `Smuggler2` rewrites the existing imports, rather than attempting to prune
  them. (This is a more aggressive approach than `smuggler` which focuses on
  removing redundant imports.) It has advantages and disadvantages. The
  advantage is that a minimal set of imports is generated in a reproducable
  format. So you can just import a library without specifying any specific
  imports and `Smuggler2` will add an explict list of things that are used from
  it. This can be a useful check and better document your modules. The
  disdvantage is that imports may be reordered, comments and blank lines
  dropped, external imports mixed with external, etc.

- By default `Smuggler2` does not remove imports completely because an import
  may be being used to only import instances of typeclasses, So it will leave
  stubs like

  ```haskell
  import Mod ()
  ```

  that you may want to remove manually. Alternatively use the `MinimiseImports`
  option to remove them anyway, at the risk of producing code that fails to
  compile.

- CPP files will not be processed correctly: the imports will be generated for
  current CPP settings and any CPP annotations in the import block will be
  discarded. This may be a particular problem if you are writing code for
  several generations of `ghc` and `base` for example. Nevetheless, `Smuggler2`
  will generate a new CPP preprocessed output file with a `-cpp` suffix.
  [retrie](https://github.com/facebookincubator/retrie/blob/master/Retrie/CPP.hs)
  solves this problem generating all possible versions of the module
  (exponential in the number of `#if` directives), operating on each version
  individually, and splicing results back into the original file. A tour de
  force!

- `smuggler2` depends on the current `ghc` compiler and `base` library to check
  whether an import is redundant. Different versions of the compiler may, of
  course, need different slightly imports, typically from `base`. The
  [base library changelog](https://hackage.haskell.org/package/base/changelog)
  provides some details of what was made available when.

- Multiple separate import lines referring to the same library are not
  consolidated

- Literate Haskell `.lhs` files will procssed into ordinary haskell files wth a
  `-lhs` suffix.

* `hiding` clauses may not be properly analysed. So hiding things that are not
  used may not be spotted.

* The test suite does not seem to run reliably on Windows. This is probably more
  of an issue with the way that the tests are run, than `Smuggler2` itself.

* Currently `cabal` does not have a particular way of specifying plugins. (See,
  eg, https://gitlab.haskell.org/ghc/ghc/issues/11244 and
  https://github.com/haskell/cabal/issues/2965) which would allow cleaner
  separation of user code and plugin-code

## For contributors

Requirements:

- `ghc-8.6.5`, `ghc-8.8.3` and `ghc-8.10.1`: `Smuggler2` will not compile with
  earlier versions.
- The test golden values are for `ghc-8.10.1` and `ghc-8.8.3`. Some of them fail
  on `ghc-8.6.5` because it seems to need to import `Data.Bool` whereas later
  versions of GHC don't. The results compile on `ghc-8.6.5` and later anyway,
  but the imports are not as minimal for later versions as they could be.
- `cabal >= 3.0` (ideally `3.2`)

### How to build

```shell
$ cabal update
$ cabal build
```

To build with debugging:

```shell
$ cabal build -fdebug
```

Curently this just adds an `-fdump-minimal-imports` parameter to GHC
compilation.

### How to run tests

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
$ cabal run smuggler2-test -- --accept
```

to update the golden outputs to the current results of (failing) tests.

It is sometimes necessary to run `cabal clean` before running tests to ensure
that old build artefacts do not lead to misleading results.

`smuggler-test` uses `cabal exec ghc` internally to run a test. The `cabal`
command that is to be used to do that can be set using the `CABAL` environment
variable. This may be helpful for certain workflows where `cabal` is not in the
current path, or you want to add extra flags to the `cabal` command.

The test suite does not seem to run reliably on Windows

Importing a test module from another test module in the same directory is likely
to lead to race conditions as 'Tasty' runs tests in parallel and so will try to
generate the same `smuggler2` output both when the imported module is being
tested directly and when it is being processed when the importing module is
being tested. Put the imported module in a subdirectory to avoid this issue, as
the test harness only looks for tests in `test\tests` and not its
subdirectories.

## Implementation approach

`smuggler2` uses the `ghc-exactprint`
[library](https://hackage.haskell.org/package/ghc-exactprint) to modiify the
source code. The documentation for the library is fairly spartan, and the
library is not widely used, at least in publicly available code, so the use here
can, no doubt, be optimised.

The library is needed because the annotated AST that GHC generates does not have
enough information to reconstitute the original source. Some parts of the
renamed syntax tree (for example, imports) are not found in the typechecked one.
`ghc-exactprint` provides parsers that preserve this information, which is
stored in a separate `Anns` `Map` used to generate properly formatted source
text.

To make manipulation of GHC's AST and `ghc-exactprint`'s `Anns` easier,
`ghc-exactprint` provides a set of Transform functions. These are intended to
facilitate making changes to the AST and adjusting the `Anns` to suit the
changes.

> These functions are
> [said to be under heavy development](https://hackage.haskell.org/package/ghc-exactprint-0.6.3/docs/Language-Haskell-GHC-ExactPrint-Transform.html).
> It is not entirely obvious how they are intended to be used or composed. The
> approach provided by [`retrie`](https://hackage.haskell.org/package/retrie)
> wraps an AST and `Anns` into a single type that seems to make AST
> transformations easier to compose and reduces the risk of the `Anns` and AST
> getting out of sync as it is being transformed, something with which the type
> system doesn't help you since the `Anns` are stored as a `Map`.

### Imports

`smuggler2` uses GHC to generate a set of minimal imports. It

- parses the original file
- dumps the minimal exports that GHC generates and parses them back in (to pick
  up the annotations needed for printing)
- drops implicit imports (such as Prelude) and, optionally, imports that are for
  instances only
- replaces the original imports with minimal ones
- `exactPrint`s the result back over the original file (or one with a different
  suffix, if that was specified as option to `smuggler2`)

This round tripping is needed because the AST that `ghc` provides does not have
enough information in it to reconstitute the source (which is why
`ghc-exactprint` exists).

### Exports

Exports are simpler to deal with as GHC's `exports_from_avail` does the work.

## Other projects

- Smuggler2 was is a rewrite of
  [`smuggler`](https://hackage.haskell.org/package/smuggler)
- `retrie` a [code modding tool](https://hackage.haskell.org/package/retrie)
  that works with GHC 8.10.1
- `refact-global-hse` an ambitious
  [import refactoring tool](https://github.com/ddssff/refact-global-hse). This
  uses `haskell-src-exts` rather than `ghc-exactprint` and so may not work with
  current versions of GHC.
- These blog posts contain some fragments on the topic of using `ghc-exactprint`
  to manipulate import lists
  [Terser import declarations](https://www.machinesung.com/scribbles/terser-import-declarations.html)
  and [GHC API](https://www.machinesung.com/scribbles/ghc-api.html) (The site
  doesn't always seem to be up.)

## Acknowledgements

Thanks to

- Dmitrii Kovanikov and Veronika Romashkina who wrote
  [`smuggler`](https://hackage.haskell.org/package/smuggler)
- Alan Zimmerman and Matthew Pickering for
  [`ghc-exactprint`](https://hackage.haskell.org/package/ghc-exactprint)
- The ghc authors who have made the compiler internals available through an API.
