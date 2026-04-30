0.3.3
=====
* GHC 9.4.4 support

0.3.2
=====
* GHC 9.2.2 support
* Drop export executable

0.3.1
=====
* GHC 9.0.1 and 9.2.1 support
* Add `HasCallStack` to unsafe* functions.
* Banish `String` on `readMaybe` and `readEither`.

0.3.0
=====

* GHC 8.10.1 support
* Use `Protolude.ConvertText` as the default string conversion class. This
  removes partial functions when converting to/from ByteStrings.
* Provide `Protolude.Conv` as a compatibility layer for old string conversion
  interface.
* Migrated `Debug` and `Unsafe` to `Protolude.Debug` and `Protolude.Unsafe`.
* Export Unicode functions:
  - `intToDigit`
  - `isAlpha`
  - `isAlphaNum`
  - `isAscii`
  - `isControl`
  - `isDigit`
  - `isHexDigit`
  - `isLetter`
  - `isLower`
  - `isPrint`
  - `isSpace`
  - `isUpper`
* Export `MonadFail` class.
* Export `gcast` from Data.Typeable.
* Export `typeOf` from Data.Typeable.
* Export `Handler` from Control.Exception.
* Export `yield` from Control.Concurrency.
* Provide compatibility module `Protolude.Partial` as single export for unsafe
  partial functions with the same naming conventions as Prelude.

0.2.4
=====

* GHC 8.8.1 support

0.2.3
=====

* GHC 8.6.1 support
* Export `fromLeft` and `fromRight`.
* Mask `always` and `alwaysSucceeds` from STM export for stm-2.5.

0.2.2
=====

* Add explicit `witness` function for use as type witness without warnings.
  Makes undefined semantically distinguishable from type witnesses.
* Backwards compatible `Protolude.Safe` module for explicit handling of partial
  list operations.
* Export `minimumDef`, `maximumDef`.
* Looser lower-bound on Data.Kind export for GHC 8.0.x.

0.2.1
====

* Exposes `throwE` and `catchE`.
* Add `transformers-compat` for old versions of transformers that require
  `throwE`, `catchE`.
* Fix `safe` version bounds for new versions.
* Add `mapExceptT and `withExceptT`.
* Export `scanl'` and provide shim for backwards compatibility.
* Add `putErrLn`.
* Expose `RealFloat`.
* Expose `GHC.Records` exports for GHC 8.2 and above.

0.2
====

* Expose `Symbol` and `Nat` types from `GHC.TypeLits` by default.
* Switch exported `(<>)` to be from `Data.Monoid` instead of Semigroup.
* Expose `putByteString` and `putLByteString` monomorphic versions of `putStrLn` functions
* Export `genericLength` and other generic list return functions.
* Rename `msg` to `fatalErrorMessage`.
* Export `ExceptT`, `ReaderT`, and `StateT` constructors.
* Mask `displayException` from default exports.
* Mask `stToIO` from default exports.
* Export `NonEmpty` type and constructor for Base 4.9 only.
* Export `Data.Semigroup` type and functions for Base 4.9 only.
* Restrict exported symbols from ``async`` to set available in 2.0.
* Add `(&&^)`, `(||^)`, `(<&&>)`, `(<||>)`
* Expose `unzip`.
* Export `maximumMay` and `minimumMay`.
* Mask `Type` export from `Data.Kind`.
* Wrap `die` to take `Text` argument instead of `[Char]`.
* Export constructors `GHC.Generics`: `(:+:)`, `(:*:)`, and `(:.:)`.
* Expose `StablePtr`, `IntPtr` and `WordPtr` types.

0.1.9
====

* Make `sum` and `product` strict

0.1.8
=====

* ``foreach`` for applicative traversals.
* ``hush`` function for error handling.
* ``tryIO`` function for error handling.
* ``pass`` function for noop applicative branches.
* Mask ``Handler`` typeclass export.
* Mask ``yield`` function export.

0.1.7
=====

* Exports monadic ``(>>)`` operator by default.
* Adds ``traceId`` and ``traceShowId`` functions.
* Exports``reader`` and ``state``  functions by default.
* Export lifted ``throwIO`` and ``throwTo`` functions.

0.1.6
=====

* Adds uncatchable panic exception throwing using Text message.
* Removes ``printf``
* Removes ``string-conv`` dependency so Stack build works without ``extra-deps``.
* Brings ``Callstack`` machinery in for GHC 8.x.
* Removes ``throw`` and ``assert`` from ``Control.Exception`` exports.
* Removes ``unsafeShiftL`` and ``unsafeShiftR`` from ``Data.Bits`` exports.
* Reexport ``throw`` as ``unsafeThrow`` via Unsafe module.
* Hides all Show class functions. Only the Class itself is exported. Forbids custom instances that are not GHC derived.
* Export`` encodeUtf8`` and ``decodeUtf8`` functions by default.
* Adds ``unsnoc`` function.

0.1.5
=====

* Initial release.
