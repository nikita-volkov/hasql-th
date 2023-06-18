# Summary

This is a fork of the official [hasql-th](https://github.com/nikita-volkow/hasql-th) library that takes a different
path for type annotations.

Key difference:

1. Does NOT support the `?` suffixes for types to annotate `NULL` mapping to `Maybe`.
2. All core types by default are `Maybe`.
3. It supports custom types that may not be `Maybe`.
4. It does NOT need to modify the SQL (Due to the `?` suffix not being supported by postgresql) and sends the
   original SQL string to the PG server, removing the SQL generation from the critical correctness pass.
5. It uses a fork of [postgresql-syntax](https://github.com/nikita-volkow/postgresql-syntax) with support
   for non standard `?` type suffixes removed.
6. Rename to `MHasql.TH` namespace.

## Acknowledgements

This library builds on the extensive work of [Nikita Volkow](https://github.com/nikita-volkow) and would not
exists without his entire hasql ecosystem.
