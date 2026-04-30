Protolude
=========

[![Build Status](https://github.com/protolude/protolude/workflows/Cabal%20CI/badge.svg)](https://github.com/protolude/protolude/actions)
[![Build Status](https://github.com/protolude/protolude/workflows/Stack%20CI/badge.svg)](https://github.com/protolude/protolude/actions)
[![Build Status](https://github.com/protolude/protolude/workflows/Nix%20CI/badge.svg)](https://github.com/protolude/protolude/actions)
[![Hackage](https://img.shields.io/hackage/v/protolude.svg)](https://hackage.haskell.org/package/protolude)

A sensible starting Prelude for building custom Preludes.

Design points:

* Banishes String.
* Banishes partial functions.
* Compiler warning on bottoms.
* Polymorphic string IO functions.
* Polymorphic show.
* Automatic string conversions.
* Types for common data structures in scope.
* Types for all common string types (Text/ByteString) in scope.
* Banishes impure exception throwing outside of IO.
* StateT/ReaderT/ExceptT transformers in scope by default.
* Foldable / Traversable functions in scope by default.
* Unsafe functions are prefixed with "unsafe" in separate module.
* Compiler agnostic, GHC internal modules are abstracted out into Base.
* ``sum`` and ``product`` are strict by default.
* Includes Semiring for GHC >= 7.6.
* Includes Bifunctor for GHC >= 7.6.
* Includes Semigroup for GHC >= 7.6.

Supports:

 * GHC 7.6.3
 * GHC 7.8.4
 * GHC 7.10.3
 * GHC 8.0.2
 * GHC 8.2.2
 * GHC 8.4.1
 * GHC 8.4.4
 * GHC 8.6.1
 * GHC 8.6.4
 * GHC 8.6.5
 * GHC 8.8.1
 * GHC 8.10.1
 * GHC 9.0.1
 * GHC 9.2.1

Stack LTS:

* lts-6.x
* lts-7.x
* lts-8.x
* lts-9.x
* lts-10.x
* lts-11.x
* lts-12.x
* lts-13.x
* lts-14.x
* lts-15.x
* lts-16.x
* lts-17.x
* lts-18.x
* lts-19.14 and higher

Usage
-----

To try out standalone prelude at the interactive shell, from the Protolude
project directory run.

```haskell
$ stack repl
> import Protolude
```

Swapping out the old Prelude
----------------------------

Disable the built-in prelude at the top of your file:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

Or directly in your project cabal file:

```haskell
default-extensions: NoImplicitPrelude
```

Then in your modules:

```haskell
import Protolude
```

Dependencies
------------

Protolude tries to be light on dependencies and only pulls in *essential*
libraries that are universally common across most real-world projects. Lower and
upper bounds are fully specified and compatible with both vanilla Cabal and
tracks Stack LTS resolver.

| Dependencies        | Lower (>=) | Upper (<) |
| -----------         |   -------- |  -------- |
| array               |        0.4 |       0.6 |
| async               |        2.0 |       2.3 |
| base                |        4.6 |      4.16 |
| bytestring          |       0.10 |      0.11 |
| containers          |        0.5 |       0.7 |
| deepseq             |        1.3 |       1.5 |
| ghc-prim            |        0.3 |       0.7 |
| hashable            |        1.2 |       1.4 |
| mtl                 |        2.1 |       2.3 |
| stm                 |        2.4 |       2.6 |
| text                |        1.2 |       1.3 |
| transformers        |        0.4 |       0.6 |
| fail                |        4.9 |      4.10 |

Structure
---------

Protolude's main modules are the following:

* [Protolude.hs](https://github.com/sdiehl/protolude/blob/master/src/Protolude.hs)
* [Base.hs](https://github.com/sdiehl/protolude/blob/master/src/Protolude/Base.hs)
* [Show.hs](https://github.com/sdiehl/protolude/blob/master/src/Protolude/Show.hs)
* [Conv.hs](https://github.com/sdiehl/protolude/blob/master/src/Protolude/Conv.hs)

FAQs
----

* **My ``putStrLn`` and ``putStr`` instances are no longer inferred in the presense
of the ``-XOverloadedStrings`` extension?**

Because the print functions are polymorphic the type of the print functions may
require annotations if the type is not fully specified by inference. To force a
specific type at the call site use either 

```haskell
putText :: MonadIO m => T.Text -> m ()
putLText :: MonadIO m => TL.Text -> m ()
```

* **How do I write manual Show instances if ``show`` isn't provided?**

Generally speaking writing manual instances of Show is a
[Haskell antipattern](http://www.stephendiehl.com/posts/strings.html) because it produces
law-violating instances of Show. You probably want to use a
[pretty printer](https://hackage.haskell.org/package/wl-pprint-text) library for custom printing.

If backwards compatibility is needed then the base library can be imported
manually.

```haskell
import GHC.Show (Show(..))
```

Automatic deriving of ``Show`` for your types is still supported since the class
is in scope by default.

* **Partial functions like ``undefined`` raise compiler warnings on
  usage.**

This is by design. For fatal uncatchable errors use the provided ``panic``
function if you intend the program to immediately abort.

```haskell
panic "Thus I die. Thus, thus, thus. Now I am dead"
```

If inside of IO simply use ``throwIO`` for exception handling, or if in pure
business logic use well-typed checked exceptions of the ``ExceptT`` variety.

* **Why is ``id`` not in scope?**

It has been renamed to ``identity`` to reserve the ``id`` identifier for the
more common use case of business logic.

* **But what if I want the partial functions?**

You if you need partial functions for backwards compatibility you can use the
`Protolude.Partial` module and mask the safe definitions as needed.

```haskell
import Protolude hiding (head)
import Protolude.Partial (head)
```

Development Tools
-----------------

**GHC Magic**

To build the `exports` management tool use:

```bash
$ cabal new-build exports --flag dev
$ cabal run exports
```

This tool uses GHC's internal compile symbol table to generate a list of exports
and keep the export list of protolude stable across different versions of GHC
and base.

**Continious Integration**

There is a massive test suite that tests all versions of GHC 7.6 - GHC HEAD
alongside all Stack resolvers to ensure no regressions. Any pull requests or
patch has to pass the 47 integrity checks before being considered. Any pull
request must keep the export list consistent across GHC and Base version and not
have any accidental symbol dropping or drift without updating the export golden
tests.

License
-------

Released under the MIT License.
Copyright (c) 2016-2022, Stephen Diehl
