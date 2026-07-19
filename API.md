# API Guide

This document groups the public `:cl-parser-kit` exports by concern and shows
the normal entry points for each layer.

For the exact symbol list, see [`src/package.lisp`](./src/package.lisp).
For parser-construction guidance organized by grammar shape, see
[`PARSING_PATTERNS.md`](./PARSING_PATTERNS.md).

## Spans

Use spans when you need source positions that survive tokenization, parsing,
and diagnostics.

- type: `span`
- constructors: `make-span`
- accessors: `span-source`, `span-start`, `span-end`, `span-start-line`,
  `span-start-column`, `span-end-line`, `span-end-column`
- helpers: `span-length`, `span-empty-p`, `span-merge`

## Tokens

Tokens carry lexical meaning and source metadata.

- type: `token`
- constructor: `make-token`
- accessors: `token-type`, `token-text`, `token-value`, `token-metadata`,
  `token-span`, `token-start`, `token-end`
- when tokens come from an external pipeline, `token-metadata` may carry
  plist-style `(:source <string>)`; diagnostics use it together with
  `token-start` / `token-end` to recover line/column data when `token-span`
  is absent

Typical usage starts with a tokenizer and ends with a vector of tokens.

- [`examples/tokenizer-example.lisp`](./examples/tokenizer-example.lisp)
- [`examples/token-stream-example.lisp`](./examples/token-stream-example.lisp)
- [`examples/external-token-diagnostic-example.lisp`](./examples/external-token-diagnostic-example.lisp)

## Tokenizers

Tokenizer helpers keep the lexical layer independent and REPL-friendly.

- tokenizer type: `tokenizer`
- constructor: `make-tokenizer`
- tokenizer accessors: `tokenizer-rules`
- rule type: `token-rule`
- rule constructor: `make-token-rule`
- rule accessors: `token-rule-type`, `token-rule-matcher`, `token-rule-skip-p`
- built-in rules: `make-literal-rule`, `make-keyword-rule`,
  `make-whitespace-rule`,
  `make-identifier-rule`, `make-number-rule`, `make-string-rule`,
  `make-predicate-rule`, `make-line-comment-rule`, `make-block-comment-rule`
- entry points: `tokenize`, `tokenize-string`

`make-literal-rule` performs raw prefix matching and is a good fit for
punctuation and operators. Use `make-keyword-rule` when a reserved word should
match only at identifier boundaries, such as `let`.

`make-identifier-rule` accepts `:start-predicate` and `:continue-predicate`
for languages whose identifiers allow sigils or suffix markers. When reserved
words should respect that same custom alphabet, pass the matching
`identifier-char-predicate` to `make-keyword-rule`.

## Diagnostics

Diagnostics and parse failures preserve structured error data.

- diagnostic type: `diagnostic`
- constructor: `make-diagnostic`
- accessors: `diagnostic-kind`, `diagnostic-message`, `diagnostic-span`,
  `diagnostic-notes`, `diagnostic-fixes`, `diagnostic-data`
- render helper: `diagnostic->string` renders the main message, opt
  source excerpt, notes, and fix-it hints in a readable multiline form
- convenience constructors: `warning-diagnostic`, `error-diagnostic`,
  `note-diagnostic`, `fix-it`, `make-fix-it`
- fix-it accessors: `fix-it-span`, `fix-it-replacement`
- parse failure helpers: `make-parse-failure`, `parse-failure-position`,
  `parse-failure-expected`, `parse-failure-actual`,
  `parse-failure-committed-p`,
  `parse-failure-diagnostics`, `parse-failure->string`,
  `merge-parse-failures`
- `parse-failure->string` is the stable top-level renderer for parse
  failures; it joins attached diagnostics when present and synthesizes a
  readable fallback message when only `expected` / `actual` data is available
- `parse-all` trailing-token failures preserve the actual trailing token and
  attach a diagnostic from the token span, falling back to `token-start` /
  `token-end` offsets or the current parser position when full span data is
  unavailable
- if that fallback path also has plist-style `(:source <string>)` in
  `token-metadata`, the synthesized diagnostic span includes reconstructed
  line/column positions and a renderable source excerpt

- [`examples/diagnostic-example.lisp`](./examples/diagnostic-example.lisp)
- [`examples/external-token-diagnostic-example.lisp`](./examples/external-token-diagnostic-example.lisp)

## Parser Primitives

The parser combinators operate on token vectors and return parse results with a
failure object on error.

- parser object: `parser`, `make-parser`, `parser-name`, `parser-fn`
- execution: `run-parser`, `parse-tokens`, `parse-all`, `parse-source`,
  `parse`
- matching: `literal`, `type-token`, `satisfies-token`
- token projection helpers: `literal-text`, `literal-value`,
  `type-token-text`, `type-token-value`
- composition: `seq`, `alt`, `many`, `many1`, `chainl1`, `chainr1`, `opt`,
  `label`, `sep-by`, `sep-by1`, `sep-end-by`, `sep-end-by1`, `preceded-by`, `terminated-by`,
  `between`, `delimited-sep-by`, `delimited-sep-by1`, `delimited-sep-end-by`,
  `delimited-sep-end-by1`, `operator-parser`,
  `lookahead`, `not-followed-by`
- functional combinators: `map-parser`, `bind-parser`, `return-parser`
- termination: `end-of-input`
- token navigation: `peek-token`, `next-token`, `eof-token-p`
- `end-of-input` and `not-followed-by` attach diagnostics from the failing
  token span, falling back to `token-start` / `token-end` offsets when span
  data is unavailable, or to the current parser position as a last resort
- the same fallback path reconstructs multiline locations when the failing
  token carries `(:source <string>)` in `token-metadata`
- `alt` propagates the farthest branch failure; when multiple
  branches fail at that same farthest position, their expected forms are merged
- `lookahead` keeps the input position unchanged on success, while preserving
  the nested farthest failure position on error
- `opt`, `many`, and `sep-by` only recover from non-consuming failures; once a
  nested parser has committed input, the original failure is propagated
- `sep-end-by` and `sep-end-by1` mirror that recovery model, but treat a final
  separator plus a non-committing item failure as a successful trailing
  separator
- when that recovery carries diagnostics, observe them through `run-parser`;
  terminal entry points (`parse-tokens`, `parse-all`, `parse-source`,
  `parse-pratt-all`) only surface terminal parse failures
- `preceded-by` and `terminated-by` are thin value-projection wrappers over
  `bind-parser` / `map-parser`; they remove delimiter boilerplate without
  changing failure positions or commitment behavior
- `delimited-sep-by` and `delimited-sep-by1` are thin wrappers over
  `between` plus `sep-by` / `sep-by1`, so they inherit the same commitment and
  failure-position behavior
- `delimited-sep-end-by` and `delimited-sep-end-by1` are the corresponding
  wrappers over `between` plus `sep-end-by` / `sep-end-by1`
- `literal-text`, `literal-value`, `type-token-text`, and `type-token-value`
  are thin wrappers over `literal` / `type-token` plus `map-parser`, so they
  keep the underlying matcher failure behavior intact
- `operator-parser` is the same kind of thin wrapper over `map-parser`; it is
  intended for `chainl1` / `chainr1` operator parsers that should ignore the
  matched token and return a binary combiner function
- `(alt)` is defined and fails cleanly with `:alternative` instead of signaling

Example:

```lisp
(let ((parser (cl-parser-kit:seq
               (cl-parser-kit:type-token :identifier)
               (cl-parser-kit:opt (cl-parser-kit:type-token :number))
               (cl-parser-kit:end-of-input))))
  parser)
```

```lisp
(cl-parser-kit:label
 (cl-parser-kit:type-token :identifier)
 :binding-name)
```

- [`examples/combinator-example.lisp`](./examples/combinator-example.lisp)
- [`examples/token-stream-example.lisp`](./examples/token-stream-example.lisp)
- [`examples/mini-language-parser.lisp`](./examples/mini-language-parser.lisp)

## Pratt Parsing

Pratt parsing is the best fit when you need expression precedence without a
large grammar framework.

- table: `pratt-table`, `make-pratt-table`
- entry types: `pratt-prefix-entry`, `pratt-infix-entry`, `pratt-postfix-entry`
- entry constructors: `make-pratt-prefix-entry`, `make-pratt-infix-entry`,
  `make-pratt-postfix-entry`
- accessors: `pratt-table-prefixes`, `pratt-table-infixes`,
  `pratt-table-postfixes`
- registration: `register-prefix-operator`, `register-infix-operator`,
  `register-postfix-operator`
- parse entry points: `parse-pratt`, `parse-pratt-all`,
  `parse-pratt-source`
- `parse-pratt` and `parse-pratt-all` accept `:position` to start from a
  later token and `:min-binding-power` to parse only operators at or above a
  precedence floor
- `parse-pratt-source` passes the same `:position`
  and `:min-binding-power` keywords through after tokenization
- EOF and missing-prefix failures return `parse-failure` values instead of
  signaling internal errors
- large or deeply nested input is bounded by `*maximum-pratt-recursion-depth*`:
  once recursion (nesting depth, or the number of operators in a flat chain)
  exceeds it, parsing returns a `:maximum-recursion-depth` failure instead of
  exhausting the control stack; rebind it for intentionally large or deep
  grammars
- `parse-pratt-all` matches `parse-all`: it rejects trailing tokens while
  preserving the actual trailing token in the returned failure
- Pratt handlers use the same parser contract as other combinators:
  prefix/postfix/infix handlers return `(values t value next nil)` on success
  and may return `(values nil nil next failure)` for domain-specific failures

- [`examples/expression-parser.lisp`](./examples/expression-parser.lisp)
- [`examples/diagnostic-example.lisp`](./examples/diagnostic-example.lisp)

## Trees

AST and CST helpers keep tree-shaped output simple and explicit.

- AST: `ast-node`, `make-ast-node`, `ast-node-type`, `ast-node-value`,
  `ast-node-children`, `ast-node-span`, `ast-node-data`, `ast-node->sexp`
- CST: `cst-node`, `make-cst-node`, `cst-node-type`, `cst-node-value`,
  `cst-node-children`, `cst-node-span`, `cst-node-data`, `cst-node->sexp`

## Testing

The repository test system runs on `cl-weave`, with `cl-prolog/weave`
providing declarative contract checks for parser behavior.

- primary entry point: `asdf:test-system "cl-parser-kit-test"`
- raw checkout script: `sbcl --script scripts/run-tests.lisp`
- coverage script: `sbcl --script scripts/run-coverage.lisp`

`cl-parser-kit-test.asd` executes the full suite through `cl-weave:run-all`
with the `:spec` reporter and treats an empty suite as a failure.

## Recommended Entry Points

If you are new to the library, start here:

1. tokenize source with `make-tokenizer` and `tokenize-string`
2. build token parsers with `seq`, `alt`, `many`, and `opt`
   For comma-separated or bracketed forms, start with `sep-by`, `sep-by1`,
   `sep-end-by`, `sep-end-by1`, `preceded-by`, `terminated-by`, `between`,
   `delimited-sep-by{,1}`, and `delimited-sep-end-by{,1}` instead of
   hand-rolling the control flow.
   For left- or right-associative operator chains outside Pratt parsing,
   start with `chainl1` or `chainr1`; pair them with `operator-parser` when
   the operator token itself does not carry semantic payload. `chainr1`
   recurses in step with the right-associative nesting depth, so for input
   that may be adversarially deep prefer `parse-pratt`, whose depth is bounded
   by `*maximum-pratt-recursion-depth*`.
3. use `parse-tokens`, `parse-all`, or `parse-source` for end-to-end parsing
4. move to `parse-pratt`, `parse-pratt-all`, or `parse-pratt-source`
   when expression precedence matters
5. use `make-ast-node` or `make-cst-node` to shape downstream data
6. use `ast-node->sexp` or `cst-node->sexp` when you need stable,
   printable tree output for tests, examples, or REPL inspection
7. read [`PARSING_PATTERNS.md`](./PARSING_PATTERNS.md) when you need to choose
   between seq helpers, projected token payloads, operator-chain helpers,
   and Pratt parsing

## Quick Start Surface

`README.md` should mirror this exact bullet list in its public API section so
the onboarding surface stays stable across both entry-point documents.

- `make-span`
- `make-token`
- `make-tokenizer`
- `tokenize`
- `tokenize-string`
- `make-diagnostic`
- `parse-tokens`
- `parse-all`
- `parse-source`
- `parse-pratt`
- `parse-pratt-all`
- `parse-pratt-source`
- `seq`
- `alt`
- `many`
- `many1`
- `opt`
- `delimited-sep-by`
- `delimited-sep-by1`
- `delimited-sep-end-by`
- `delimited-sep-end-by1`
- `make-ast-node`
- `make-cst-node`
- `ast-node->sexp`
- `cst-node->sexp`

## Parser Entry Points

End-to-end entry points intentionally stay small so combinator logic, Pratt
logic, and tokenizer construction remain separable:

- `parse-tokens` accepts a parser and a token vector, and does not require the
  parse to consume every token
- `parse-all` accepts a parser and a token vector, and enforces full
  consumption
- `parse-source` tokenizes a source string with a tokenizer, then delegates to
  `parse-all`
- `parse-pratt` parses a precedence expression from a token vector
- `parse-pratt-all` parses a precedence expression and enforces full
  consumption
- `parse-pratt-source` tokenizes a source string, then delegates to
  `parse-pratt-all`
