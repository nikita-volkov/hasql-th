https://api.travis-ci.org/nikita-volkov/hasql-th.svg?branch=0.3/master

# Summary

A Hasql extension-library, which brings compile-time syntax checking of queries atop of a great simplification of declaration of statements. All using quasi-quotes and a port of Postgres SQL syntax parser.

Here's a brief example of how it works:

selectUserDetails :: Statement Int32 (Maybe (Text, Text, Maybe Text))
selectUserDetails =
  [maybeStatement|
    select name :: text, email :: text, phone :: text?
    from "user"
    where id = $1 :: int4
    |]

As you can see, it completely eliminates the need to mess with codecs. The quasiquoters directly produce `Statement`, which you can then `dimap` over using its `Profunctor` instance to map to your domain types.

You can get the latest development version of the project here: https://github.com/nikita-volkov/hasql-th/tree/0.3/master. With Stack, you can directly depend on the latest commit there.

# Status

This is a closed-beta version, which brings limited functionality. At the moment the library only supports the Select statements. However support for other types of statements is close to being finished. It's only a matter of porting the remaining pieces of the parser and AST. Since the syntax tree is highly shared, most of their pieces have already been ported, during the work on Select.

# Quality

The parser and renderer get heavily tested using the following property: rendering a random AST then parsing it should produce the same AST. This pretty much covers most possible reasons for bugs in the library.

# Why not use the original Postgres parser?

Unfortunately Postgres doesn't export it's own parser in any of its distribution, so there's no C-library to link to and wrap.

Isolating the original C-code and including it in a Haskell project is also not an option, because it's heavily based on code generators and a complex make-file instructions. Maintaining such a codebase also seems like a non-viable option.

Fortunately the original parser is implemented using a declarative notation (the one which the mentioned code generators work with). It being declarative makes the process of porting to Haskell quite straight-forward. 

For these reasons it's been decided to port the original parser and AST as close as possible to Haskell using the "megaparsec" library.

# The unexpected goodies

The parser turns out to be actually better than the one in Postgres in terms of error-reporting. That's because instead of relying on an old C-mangling transcompiler, it uses Haskell's own superpowerful "megaparsec" library and the "headed-megaparsec" extension for it. As the result of that, the error messages produced by this parser are more informative than the ones in Postgres. Following is an example.

## Parser errors example

Consider the following statement:

```sql
select 1 from a where b >= 3 && b < 4
```

It is incorrect, because it uses `&&` instead of `and`. But here's what Postgres' original parser says about it:

```
ERROR:  syntax error at or near "<"
LINE 1: select 1 from a where b >= 3 && b < 4;
                                          ^
```

Here's what "hasql-th" says:

```
  |
2 |     select 1 from a where b >= 3 && b < 4;
  |                                  ^
unexpected '&'
```
