• # cl-parser-kit Implementation Prompt

  You are an expert Common Lisp library author and OSS maintainer.

  Create a new OSS Common Lisp library named `cl-parser-kit`.

  ## Goal

  `cl-parser-kit` is a small, general parser toolkit for Common Lisp.

  It should provide a practical foundation for:

  - tokenization
  - parsing
  - parser combinators or parser primitives
  - Pratt parsing
  - source spans and diagnostics
  - incremental or structured parsing where useful
  - AST/CST construction helpers
  - error reporting for text-based languages

  This is not an application framework, not a CLI framework, and not a full compiler frontend.

  ## Design Principles

  - Keep the core small.
  - Prefer simple, composable primitives over a giant parser framework.
  - Avoid app-specific concepts.
  - Avoid unnecessary dependencies.
  - Public exported symbols are API and must be intentional.
  - Every public behavior must have tests.
  - Make examples minimal and practical.
  - Design for SBCL first, but avoid SBCL-specific code unless isolated.
  - Use ASDF systems and packages cleanly.
  - Do not create a catch-all `utils` package.
  - Avoid excessive comments.

  ## Non-Goals

  Do not implement:

  - CLI framework
  - terminal/TTY handling
  - Prolog engine
  - event system
  - dataflow engine
  - generic utility package
  - large compiler infrastructure
  - full language workbench
  - editor integration

  ## Repository Structure

  Create this structure:

  ```text
  cl-parser-kit/
    README.md
    LICENSE
    cl-parser-kit.asd
    cl-parser-kit-test.asd
    src/
      package.lisp
      core.lisp
      tokens.lisp
      tokenizer.lisp
      spans.lisp
      diagnostics.lisp
      combinators.lisp
      pratt.lisp
      parser.lisp
      ast.lisp
      cst.lisp
      testing.lisp
    t/
      package.lisp
      tokens-test.lisp
      tokenizer-test.lisp
      spans-test.lisp
      diagnostics-test.lisp
      combinators-test.lisp
      pratt-test.lisp
      parser-test.lisp
    examples/
      tokenizer-example.lisp
      expression-parser.lisp
      mini-language-parser.lisp

  ## ASDF Systems

  Define these systems:

  :cl-parser-kit
  :cl-parser-kit-test

  The test system should support:

  (asdf:test-system :cl-parser-kit)

  Keep dependencies light. If a test framework is needed, prefer FiveAM, but do not make the library itself depend on heavy infrastructure.

  ## Public Package

  Create one primary public package:

  :cl-parser-kit

  Internal packages are allowed only if they clearly reduce complexity.

  Export only intentional API symbols.

  ## Core Concepts

  ### Token

  A token represents a lexical unit.

  A token should have:

  - type
  - lexeme/text
  - start position
  - end position
  - metadata

  ### Span / Source Location

  Provide source span support for:

  - line
  - column
  - offset
  - start/end positions

  This should be usable in diagnostics and AST nodes.

  ### Tokenizer

  Provide a practical tokenizer API.

  Support:

  - token stream creation
  - whitespace handling
  - comments if useful
  - custom token predicates or token rules
  - end-of-input handling

  The tokenizer should be usable independently.

  ### Parser

  Provide a parser API that can consume token streams or character streams.

  Support:

  - basic parser primitives
  - parser composition
  - sequencing
  - choice / alternation
  - repetition
  - optional parsing
  - lookahead if simple
  - failure reporting

  ### Pratt Parser

  Provide a Pratt parser helper for expression parsing.

  Support:

  - prefix operators
  - infix operators
  - postfix operators if simple
  - precedence and associativity
  - AST construction hooks

  ### Diagnostics

  Provide structured parse errors.

  Support:

  - error kind
  - message
  - span
  - expected tokens or forms
  - actual token or lexeme where useful

  ### AST / CST Helpers

  Provide helpers for structured tree construction.

  Support:

  - nodes
  - node type
  - children
  - metadata
  - spans
  - pretty inspection helpers if useful

  ### Testing Helpers

  Provide helpers for:

  - parsing input and asserting AST shape
  - asserting token sequences
  - asserting diagnostics
  - asserting span correctness

  ## Suggested Public API

  Use this as a starting point:

  make-token
  token-type
  token-text
  token-start
  token-end

  make-span
  span-start
  span-end
  span-line
  span-column

  make-diagnostic
  diagnostic-message
  diagnostic-span
  diagnostic-expected
  diagnostic-actual

  tokenize
  tokenize-string
  peek-token
  next-token
  eof-token-p

  parse
  parse-tokens
  parse-source

  alt
  seq
  opt
  many
  many1

  make-pratt-table
  register-prefix-operator
  register-infix-operator
  register-postfix-operator
  parse-expression

  make-ast-node
  ast-node-type
  ast-node-children
  ast-node-span

  make-cst-node
  cst-node-type
  cst-node-children
  cst-node-span

  You may adjust names if the resulting API is cleaner, but keep it small and coherent.

  ## Examples

  Create runnable examples for:

  ### Tokenizer Example

  Tokenize a simple expression or config snippet.

  ### Expression Parser

  Parse expressions like:

  1 + 2 * 3

  ### Mini Language Parser

  Parse a tiny language with:

  - identifiers
  - numbers
  - operators
  - parentheses
  - simple statements

  The examples should be REPL-friendly.

  ## README Requirements

  Write an OSS-quality README with:

  - project description
  - installation using ASDF/local-projects
  - basic usage example
  - core concepts
  - API overview
  - testing instructions
  - design non-goals
  - license

  The README must clearly state that cl-parser-kit is about parsing primitives and structured parsing, not CLI, terminal handling, Prolog, or dataflow.

  ## Tests

  Write tests for:

  - token creation
  - span creation
  - tokenizer behavior
  - EOF handling
  - parse success cases
  - parse failure cases
  - diagnostics content
  - combinator behavior
  - Pratt parsing precedence
  - Pratt associativity
  - AST/CST construction
  - span preservation across parsed structures

  Tests should be runnable with:

  (asdf:test-system :cl-parser-kit)

  ## Verification

  Before finishing:

  - Ensure ASDF systems load cleanly.
  - Ensure tests run.
  - Ensure README examples match implemented API.
  - Ensure exported symbols are intentional.
  - Ensure there are no circular package dependencies.
  - Ensure code is formatted consistently.
  - Keep implementation simple enough to understand from tests.

  ## Deliverable

  Create the complete repository files.

  After implementation, report:

  - files created
  - public API symbols
  - how to run tests
  - intentionally deferred features
