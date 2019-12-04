# Summary

A Hasql extension-library, which brings compile-time syntax checking of queries atop of a great simplification of declaration of statements. All using quasi-quotes.

Here's a brief example of how it works:

```haskell
selectUserDetails :: Statement Int32 (Maybe (Text, Text, Maybe Text))
selectUserDetails =
  [maybeStatement|
    select name :: text, email :: text, phone :: text?
    from "user"
    where id = $1 :: int4
    |]
```

As you can see, it completely eliminates the need to mess with codecs. The quasiquoters directly produce `Statement`, which you can then `dimap` over using its `Profunctor` instance to map to your domain types.

# Status

The library supports almost all of Postgresql syntax in Select, Insert, Update and Delete statements, with an exception of some very rarely used features, such as XML-related functionality.

## Quality

[![Build Status](https://travis-ci.org/nikita-volkov/hasql-th.svg?branch=master)](https://travis-ci.org/nikita-volkov/hasql-th)

The parser and renderer get heavily tested using the following property: rendering a random AST then parsing it should produce the same AST. This pretty much covers most possible reasons for bugs in the library.

# Implementation

This library internally implements a port of the original Postgres SQL syntax parser. It might sound like an overkill, but there really were no better options.

<details>
<summary>Why not reuse the parser?</summary>

Unfortunately Postgres doesn't export it's own parser in any of its distributions, so there's no C-library to link to and wrap.

Isolating the original C-code and including it in a Haskell project is also not an option, because it's heavily based on code generators and complex make-file instructions. Maintaining such a codebase also seems like a non-viable option.

Fortunately the original parser is implemented using a declarative notation (the one which the mentioned code generators work with). It being declarative makes the process of porting to Haskell quite straight-forward. 

Also for the purposes of this library we need access to the full syntax tree for extracting data on placeholders and statement results. Quick and dirty hacks won't do.

For these reasons it's been decided to port the original parser and AST as close as possible to Haskell using the "megaparsec" library.

</details>

# Error messages

The parser turns out to be actually better than the one in Postgres in terms of error-reporting. That's because instead of relying on an old C-mangling transcompiler, it uses Haskell's own superpowerful "megaparsec" library and the "headed-megaparsec" extension for it. As the result of that, the error messages produced by this parser are more informative than the ones in Postgres. Following is an example.

## Error example 1

Consider the following broken statement:

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

## Error example 2

It's not obvious what is wrong in the following statement either:

```sql
insert into user (name) values ($1)
```

The Postgres parser doesn't help much:

```
ERROR:  syntax error at or near "user"
LINE 1: insert into user (name) values ($1);
                    ^
```

Here's what "hasql-th" says though:

```
  |
2 |       insert into user (name) values ($1)
  |                       ^
Reserved keyword "user" used as an identifier. If that's what you intend, you have to wrap it in double quotes.
```

## Error example 3

It turns out that the original Postgres parser never produces any other messages than the opaque "syntax error at or near". "hasql-th" on the other hand is quite descriptive. E.g., here's how it gradually guides to insert the missing expected pieces.

Input:

```haskell
[resultlessStatement|insert into |]
```

Error:

```
  |
1 | insert into 
  |             ^
unexpected end of input
expecting identifier or white space
```

Input:

```haskell
[resultlessStatement|insert into a |]
```

Error:

```
  |
1 | insert into a 
  |               ^
unexpected end of input
expecting "default", "overriding", "select", "values", '(', white space, or with clause
```
