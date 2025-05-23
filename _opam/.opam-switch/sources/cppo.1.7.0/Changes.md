## v1.7.0 (2024-08-22)
- [+ui] Multi-line macros, without line terminators `\`,
        can now be defined using `#def` and `#enddef`.
        These macro definitions can be nested.
- [+ui] Higher-order macros:
        a macro can now take a parameterized macro as a parameter.
- [compat] Better locations for some syntax error messages.

## v1.6.9 (2022-05-19)
- [bug] Fix multiline string support (#81)

## v1.6.8 (2021-09-17)
- [compat] Allow version strings without patch numbers, _e.g._ `8.13+beta1`
           The patch number will be set to 0 upon empty, _i.e._ `(8, 13, 0)`

## v1.6.7 (2020-12-21)
- [compat] Treat ~ and - the same in semver in order to parse
           OCaml 4.12.0 pre-release versions.
- [compat] Restore 4.02.3 compatibility.

## v1.6.6 (2019-05-27)
- [pkg] port build system to dune from jbuilder.
- [pkg] upgrade opam metadata to 2.0 format.
- [pkg] remove topkg and use dune-release.
- [compat] Use `String.capitalize_ascii` to remove warning.

## v1.6.5 (2018-09-12)
- [bug] Fix 'asr' operator (#61)

## v1.6.4 (2018-02-26)
- [compat] Tests should now work with older versions of jbuilder.

## v1.6.3 (2018-02-21)
- [compat] Fix tests.

## v1.6.1 (2018-01-25)
- [compat] Emit line directives always containing the file name,
           as mandated starting with ocaml 4.07.

## v1.6.0 (2017-08-07)
- [pkg] BREAKING: cppo and cppo_ocamlbuild are now two distinct opam
        packages.

## v1.5.0 (2017-04-24)
- [+ui] Added the `CAPITALIZE()` function.

## v1.4.0 (2016-08-19)
- [compat] Cppo is now safe-string ready.

## v1.3.2 (2016-04-20)
- [pkg] Cppo can now be built on MSVC.

## v1.3.1 (2015-09-20)
- [bug] Possible to have #endif between two matching parenthesis.

## v1.3.0 (2015-09-13)
- [+ui] Removed the need for escaping commas and parenthesis in macros.
- [+ui] Blanks is now allowed in argument list in macro definitions.
- [+ui] #directive with wrong arguments is now giving a proper error.
- [bug] Fixed expansion of __FILE__ and __LINE__.

## v1.1.2 (2014-11-10)
- [+ui] Ocamlbuild_cppo: added the ocamlbuild flag `cppo_V(NAME:VERSION)`,
        equivalent to `-V NAME:VERSION` (for _tags file).

## v1.1.1 (2014-11-10)
- [+ui] Ocamlbuild_cppo: added the ocamlbuild flag `cppo_V_OCAML`,
        equivalent to `-V OCAML:VERSION` (for _tags file).

## v1.1.0 (2014-11-04)
- [+ui] Added the `-V NAME:VERSION` option.
- [+ui] Support for tuples in comparisons: tuples can be constructed
        and compared, e.g. `#if (2 + 2, 5) < (4, 5)`.

## v1.0.1 (2014-10-20)
- [+ui] `#elif` and `#else` can now be used in the same #if-#else statement.
- [bug] Fixed the Ocamlbuild flag `cppo_n`.

## v1.0.0 (2014-09-06)
- [bug] OCaml comments are now better parsed. For example, (* '"' *) works.

## v0.9.4 (2014-06-10)
- [+ui] Added the ocamlbuild_cppo plugin for Ocamlbuild. To use it:
        `-plugin(cppo_ocamlbuild)`.

## v0.9.3 (2012-02-03)
- [pkg] New way of building the tar.gz archive.

## v0.9.2 (2011-08-12)
- [+ui] Added two predefined macros STRINGIFY and CONCAT for making
        string literals and for building identifiers respectively.

## v0.9.1 (2011-07-20)
- [+ui] Added support for processing sections of files using external programs
        (#ext/#endext, -x option)
- [doc] Moved and extended documentation into the README file.

## v0.9.0 (2009-11-17)

- initial public release
