# FDB: A Functional Database written in Rust

## What is FDB?

FDB is  a functional database written in Rust.

_Functional_ means that the query language of the database is based on functions.
That is, filters, projections, aggregations, etc... are all passed to the query engine as functions.

The query language itself is heavily influenced by existing functional APIs for data processing, e.g. the `List` API in Haskell or the `Stream` API in Java.
Basic operations like `filter` and `fold` are composable to generate arbirtarily complex queries.

## Goals

>_Don't we have enough databases out there already?_
>
>Yes we do.
>
>_Aren't they performant enough?_
>
>They are very performant.
>
>_Then what is the goal?_

The goal of the FDB project is not to revolutionize the field of databases or "beat" any existing systems.
It is a personal project with the following goals:

1. Devise an expressive functional query language that is:
    - more expressive than SQL
    - easier to write than SQL
    - "elegant" from a programming language designer's point of view (see section "Motivation")
2. Implement a basic database core that supports said language as its main query language
3. Get performance on par with existing SQL systems

Bear in mind this is also a personal project with the following personal goals:

1. Learn Rust by implementing a large and complex system
2. Have fun designing and implementing a database!

## Non goals

The goal of FDB **is not** to:

- Become the new PostgreSQL
- Become the new Oracle
- Be the fastest

## Motivation

> _But why?_

#### From a programming language designer's point of view, SQL is not a good language

With a modern language designer's eye, the following features of the SQL language make it a bad language:

The SQL language suffers from the following major design flaws at its core:

- **Limited/cumbersome composability**.
The `SELECT`/`FROM`/`WHERE` syntax of the language makes it cumbersome to compose small elements.
For example, to add a filter on top of an existing query block, one needs to write an entire `SELECT`/`FROM`/`WHERE` statement, even though the `WHERE` clause is the only one containing actual logic.
This makes it hard for external tools to code generate to SQL (add external refs here).
This structure is also hard to analyze directly, and is therefore torn down relatively early on in the SQL compilation process of most RDBMS systems, in favor of a simpler tree of operators closer to relational algebra and modern functional data processing APIs.
- **Context-based semantics.**
Aggregators with/without 
- **Generic reusability**. 
argmax example
- Fully iniline writing of programs
- All declarative (hard to write)
No variables thazt dan store subauery (WITH clausee now)
- no nesting
- special everything: types, operators, ...

## State of the current prototype