# Changelog

## Unreleased

## 0.1.0 - 2026-07-20

- hardened parser entry points that accept caller-supplied token streams:
  `*maximum-parser-tokens*` caps `run-parser`, `parse-tokens` / `parse-all`,
  and `parse-pratt` / `parse-pratt-all`, returning a parse failure before
  walking oversized caller-controlled token vectors or proper lists;
  `filter-tokens` signals at the same limit, circular or improper token lists
  are rejected before traversal, and `skip-until` now skips long recovery runs
  iteratively instead of consuming control stack
- added `*maximum-tree-depth*`, `*maximum-tree-nodes*`,
  `tree-depth-limit-exceeded`, `tree-node-limit-exceeded`, and
  `tree-child-list-invalid` so AST/CST traversal, conversion, comparison, and
  rendering helpers fail explicitly on adversarially deep, wide, or malformed
  external trees instead of exhausting the control stack or memory
- added predicate token-run scanners: `take-while` / `take-while1` match a run of
  consecutive tokens satisfying a predicate (returning the list; `take-while1`
  requires at least one), and `skip-while` skips such a run discarding it —
  Megaparsec's `takeWhileP` / `takeWhile1P` over a token stream
- added a CSV parser example (`examples/csv-parser-example.lisp`), a
  line-oriented counterpoint keeping the newline as a real token: rows via
  `sep-end-by`, fields via `sep-by1`, and quoted fields that may contain commas
- added an error-recovery example (`examples/error-recovery-example.lisp`)
  demonstrating panic-mode recovery with `recover` + `skip-until` driven by
  `many-till`, so one parse reports every malformed statement (and collects the
  recovery diagnostics, read from `run-parser`'s fourth value) instead of
  aborting at the first error
- added a complete recursive JSON parser example
  (`examples/json-parser-example.lisp`), an end-to-end demonstration of the
  tokenizer (escaped strings, signed/exponent numbers, keyword and literal
  rules), a self-referential grammar via `defparser`, and the
  sequence/choice/separator combinators, decoding objects to alists and arrays
  to lists
- completed the fix-it feature with applicators (previously fix-its were
  suggestion data with no way to apply them):
  - `apply-fix-it` — return source with one fix-it's span region replaced by its
    replacement (a `nil` replacement deletes it); offsets are clamped to source
  - `apply-fixes` — apply a list of fix-its last-to-first, so each edit leaves
    the not-yet-applied earlier offsets valid; same-position zero-width
    insertions preserve input order; pair with `diagnostic-fixes` to auto-apply
    a diagnostic's suggestions
- rounded out the repetition/control combinators against Parsec/Megaparsec/nom/
  FParsec:
  - `some-till` — `many-till` requiring at least one match before the end parser
    (Megaparsec's `someTill`)
  - `length-count` — parse a count for N, then parse an item parser exactly N
    times (nom's `length_count`); N is capped by
    `*maximum-parser-repetition-count*`, and each item must consume input, so a
    hostile count cannot force an unbounded loop
  - `not-empty` — fail if a parser succeeds without consuming input, to guarantee
    forward progress (FParsec's `notEmpty`)
- added opt-in packrat memoization and completed the token-matching family:
  - `memoize` / `with-parse-memoization` — wrap a parser so that, inside the
    dynamic extent, its result at each position is computed once and reused,
    turning an ambiguous or heavily backtracking grammar's exponential
    re-parsing into linear-time packrat parsing (a no-op outside the extent, so
    grammars that do not need it are unaffected)
  - `token-value-in` / `token-value-not-in` — match (or reject) a token whose
    `token-value` is one of a set, completing the type/text/value matching family
    alongside `token-type-in` / `token-text-in`
- added structured-diagnostic accessors, the counterparts of the string
  renderers:
  - `parse-failure->diagnostics` — the structured `diagnostic` objects for a
    failure (its attached diagnostics, or a synthesized default), for rendering
    or aggregating failures with your own tooling
  - `diagnostics->string` — render a whole list of diagnostics (blank-line
    separated), the multi-diagnostic form of `diagnostic->string`
- added two stream/error-handling conveniences:
  - `filter-tokens` — return a fresh vector of the tokens satisfying a predicate,
    for pruning a stream (e.g. dropping non-skipped `:comment` tokens) before
    parsing
  - `parse-failure-span` — the source span of a failure's actual token (or `nil`
    at end of input), for rendering a caret or slicing the offending region
    without building a full diagnostic
- added tree construction and serialization-round-trip helpers for both node
  families, removing the boilerplate of building located nodes by hand:
  - `token->ast-node` / `token->cst-node` — build a leaf node from a token (its
    value from a `:value-function`, `token-text` by default, and its span from
    the token)
  - `ast-node-of` / `cst-node-of` — run a parser and wrap the result into a node
    whose span covers the consumed tokens (value in `value`, or in `children`
    with `:as-children t`)
  - `sexp->ast-node` / `sexp->cst-node` — reconstruct a node from the plist that
    `ast-node->sexp` / `cst-node->sexp` produce, rebuilding an embedded span, so
    the `->sexp` form round-trips
- added tree-rendering helpers for both node families, completing the tree
  lifecycle (build, traverse, query, fold, compare, and now render):
  - `ast-node->string` / `cst-node->string` — a human-readable indented tree
    (one node per line, `type` then `value`) for debugging and REPL inspection
  - `ast-node->dot` / `cst-node->dot` — a Graphviz DOT digraph (`:graph-name`
    names the graph) for visualizing a parse tree with `dot`
- added an operator-precedence expression builder and two combinator gaps:
  - `make-expression-parser` — the combinator-layer counterpart to the Pratt
    parser: build a parser from an operator table (precedence levels highest
    first, with `:prefix` / `:postfix` / `:infix-left` / `:infix-right` /
    `:infix-non-assoc` operator specs whose parsers yield the combining
    function), for when operands and operators are arbitrary parsers rather than
    single tokens; see `examples/operator-precedence-example.lisp`
  - `sequence-of` — the list form of `seq` (`(sequence-of (list a b))` is
    `(seq a b)`), the counterpart to `choice`
  - `chain-postfix` — a left-associative suffix chain (member access, calls,
    indexing): parse a base, then fold zero or more suffix parsers, each yielding
    a function that transforms the accumulated value
- broadened the surface again with a further batch of commit-preserving
  additions, each built on the existing primitives and covered by tests:
  - backtracking control: `attempt` — the inverse of `commit`, demoting a
    parser's committed failure to a recoverable one so a surrounding
    `opt`/`many`/`sep-by` backtracks to the start position (Parsec's `try`)
  - permutation parsing: `permute` — parse a fixed set of parsers in any order,
    each exactly once, returning their values in argument order; its parser list
    is capped by `*maximum-parser-repetition-count*`
  - value shaping: `pair` and `separated-pair` (nom-style two-parser sequences),
    plus `fold-many1` (the one-or-more form of `fold-many`)
  - negative token sets: `token-type-not-in` / `token-text-not-in`, the
    complements of `token-type-in` / `token-text-in`; all token set combinators
    cap caller-supplied sets at `*maximum-parser-repetition-count*`
  - tokenizer rules: `make-radix-integer-rule` (base 2..36 with an optional
    case-insensitive prefix such as `0x`/`0b`/`0o`), `make-float-rule` (decimal
    exponents, e.g. `6.022e23`, with a clamped `*maximum-number-exponent*` guard),
    `make-operator-rule` (longest-match over an operator set), and
    `make-nested-block-comment-rule` (comments that nest); plus `:case-sensitive`
    on `make-keyword-rule` and `:escapes` decoding on `make-string-rule`
  - Pratt registrars: `register-ternary` (`cond ? then : else`, right
    associative) and `register-infix-non-assoc` (chaining is a parse error)
  - tree utilities for both node families: `ast-node-reduce` / `cst-node-reduce`
    (fold over nodes), `ast-node-equal` / `cst-node-equal` (structural equality),
    and an `:order :post` option on `ast-node-walk` / `cst-node-walk`
- expanded the combinator surface with commit-preserving additions built on the
  existing primitives:
  - choice/value: `choice` (list form of `alt`), `option` (`opt` with an
    explicit default), `fail-parser`, `as-value`, `pure`
  - repetition: `times`, `skip-many`, `skip-many1`, `fold-many`, `many-till`,
    and `chainl` / `chainr` (defaulting variants of `chainl1` / `chainr1`)
  - applicative and source spans: `seq-map` (lift a function over `seq`), `pick`
    (keep the N-th result of a sequence), `spanning` (attach the merged source
    span of the consumed tokens, for building located AST/CST nodes)
  - token matching: `any-token`, `token-type-in`, `satisfies-value`
  - failure context: `context` (append an explanatory `note-diagnostic` to a
    failure without changing its expected form or commitment)
  - tokenizer rule: `make-char-rule` (single character by character, set, or
    predicate)
  - tree queries generated for both node families: `ast-node-collect` /
    `cst-node-collect` (all matches), `ast-node-count` / `cst-node-count`,
    `ast-node-depth` / `cst-node-depth`
  - range repetition: `times-between` (greedy min..max), `at-least` and
    `at-most` (open-ended variants), plus `surrounded-by` for symmetric
    delimiters
  - high-level Pratt registrars that hide the raw nud/led protocol and
    binding-power arithmetic: `register-atom`, `register-prefix`,
    `register-infix-left`, `register-infix-right`, `register-postfix`, and
    `register-grouping` (matched `open expr close` pairs)
  - error recovery: `skip-until` and `recover` for panic-mode
    resynchronisation, so a single parse can report several errors (drive the
    loop with `(many-till statement (end-of-input))`)
  - tree traversal generated for both node families: `ast-node-walk` /
    `cst-node-walk` (pre-order visit), `ast-node-find` / `cst-node-find` (first
    match), `ast-node-map` / `cst-node-map` (bottom-up rebuild)
  - ergonomic macros: `parse-let*` (do-notation over `bind-parser`),
    `parser-lazy` and `defparser` (forward references and recursive grammars)
  - comprehensive gap closure against Parsec/Megaparsec/nom/FParsec:
    `end-by` / `end-by1` (required terminator), `verify` (assert a predicate on a
    parsed value), `commit` (PEG cut), `current-position` (capture the token
    index), `token-text-in` (match a lexeme set), `recognize` (span of consumed
    tokens), and the span helpers `span-contains-position-p` and `span-text`
- fixed a silent error-swallowing bug in `bind-parser` (the foundation of
  `preceded-by`, `terminated-by`, `between`, and the `delimited-sep-by*`
  wrappers): once the leading parser consumed input, a failing trailing parser
  was reported as a non-committed failure, so a surrounding
  `opt`/`many`/`sep-by`/`alt` backtracked past the half-consumed construct and
  dropped the grammar error. It now promotes the failure to committed, matching
  `seq`
- hardened the combinator engine against untrusted input with
  `*maximum-parser-recursion-depth*` (default 4000, checked in `run-parser` and
  in `chainr1`'s own right-recursion), so deeply nested grammars return a
  `:maximum-recursion-depth` failure instead of exhausting the control stack
- added tokenizer resource limits `*maximum-tokenizer-source-length*` and
  `*maximum-tokenizer-tokens*`, plus tokenizer rule-count and operator
  alternative-count limits, which signal the exported
  `tokenizer-resource-limit-exceeded` condition instead of exhausting memory on
  an adversarially large or token-dense source
- bounded computed parser-list construction in `choice`, `sequence-of`,
  `seq-map`, `pick`, `permute`, token set combinators, and
  `make-expression-parser`, and removed diagnostic merging through
  `apply`/`append`, so attacker-influenced grammar inputs cannot expand into
  unbounded argument lists
- bounded numeric scanning with `*maximum-number-lexeme-length*` (default 1024)
  so a multi-million-digit run cannot force superlinear bignum work; excess
  digits gracefully start a new token
- bounded diagnostic rendering with `*maximum-diagnostic-line-length*` (default
  400) so a single pathological source line (e.g. a minified file) cannot make
  one `diagnostic->string` call allocate proportional to that line's full length
- bounded rendered diagnostic notes and fix-it hints with
  `*maximum-diagnostic-related-count*` so externally constructed diagnostics
  cannot force unbounded related-item rendering; circular or improper
  related-item lists are rejected through the same condition
- bounded `apply-fixes` input entries with `*maximum-diagnostic-fix-count*`,
  including `nil` entries skipped during application, so circular or nil-only
  fix batches terminate with `diagnostic-resource-limit-exceeded`; improper fix
  lists are rejected through the same condition
- bounded batched diagnostic rendering input entries, including `nil` entries
  skipped for output, with `*maximum-diagnostic-count*` and parse-failure
  expected/diagnostic payloads with
  `*maximum-parse-failure-expected-count*` /
  `*maximum-parse-failure-diagnostic-count*`; circular or improper batched
  diagnostic and parse-failure payload lists are rejected through the same
  resource-limit conditions
- bounded `seq-map` function-call arity with `*maximum-parser-apply-arity*`,
  removing an attacker-sized `apply` path
- hardened release bootstrap dependency scanning to statically inspect ASD
  metadata instead of loading dependency ASD files, and to reject component paths
  that resolve outside their system source root
- reduced avoidable allocation in `permute`, removed duplicate child-list
  traversal from tree equality, and bucketed operator matching by leading
  character
- fixed `span-merge` to derive start/end line and column from whichever argument
  actually has the smallest start / largest end offset, so merging two spans out
  of source order no longer produces an internally inconsistent span
- fixed `%make-offset-span` normalization so a token whose raw start/end are both
  negative no longer normalizes to an inverted span
- upgraded the test-only dependencies to their latest releases (cl-weave
  `v0.8.0`, cl-prolog `v0.6.0`) and migrated to cl-weave's string-named test
  registration API
- fixed `between` (and therefore `delimited-sep-by`/`delimited-sep-end-by`)
  to return the inner value instead of the opening delimiter
- fixed `lookahead` to preserve the farthest inner-failure position instead of
  resetting to the starting position on failure
- fixed a crash on an unterminated block comment: `make-block-comment-rule` now
  consumes to end-of-source (like line comments) instead of raising a type error
  on untrusted input that opens `/*` without closing it
- fixed diagnostic caret rendering, which was drawn two columns too far right
- fixed source-line context under carets for CR-only (classic-Mac) sources so
  line splitting agrees with position tracking
- hardened `make-number-rule` against untrusted input: numbers are parsed with
  `parse-integer` instead of the Lisp reader (which permanently interned
  malformed runs as symbols), and the scanner accepts at most one decimal point
- bounded Pratt recursion with `*maximum-pratt-recursion-depth*` so
  pathologically deep input returns a `:maximum-recursion-depth` failure
  instead of exhausting the control stack
- eliminated the O(n^2) parse cost for list token streams by coercing to a
  vector once at the `parse-tokens` / `parse-all` boundary
- sped up tokenization (roughly 2x) by returning step state through multiple
  values instead of a per-token plist, plus tighter numeric declarations
- added property-based tests (parser invariants, tokenizer span coverage) and a
  cl-prolog relational contract proving the Pratt precedence graph is acyclic
- fixed `scripts/run-coverage.lisp` to compile the project (sb-cover instruments
  at compile time) so the coverage report captures data instead of failing with
  "did not capture any coverage data"; the src coverage gate now passes at
  ~91% expression / ~87% branch
- added `scripts/run-compile-check.lisp` as a raw-checkout compile-verification
  entry point for the library and test ASD systems
- added `scripts/run-examples.lisp` as a raw-checkout example verification
  entry point so shipped sample files are executable outside the test package
- documented a canonical quick-start API surface in `API.md` and pinned
  `README.md` to the same curated bullet list with a regression test
- expanded `scripts/run-release-audit.sh` so release-readiness checks now
  enforce community, security, versioning, governance, maintainer, roadmap,
  and license artifacts in addition to the verification entry points
- made `scripts/run-implementation-smoke.sh` print each implementation's exact
  command, reported runtime version, and failing exit status for more
  audit-friendly portability triage
- added `scripts/run-release-audit.sh` as a single-command release-readiness
  entry point that checks required docs before running the baseline and smoke
  verification commands
- added `scripts/run-implementation-smoke.sh` as a checked-in multi-
  implementation smoke entry point for raw-checkout portability checks
- added `scripts/run-tests.lisp` as a raw-checkout verification entry point
  that loads repository source and test files directly before running the
  checked-in test runner
- published repository metadata in both ASD files for homepage, issue tracker,
  and source-control discovery
- added `SUPPORT.md` to document the verified support boundary and release
  expectations
- expanded `SECURITY.md` so private reports include runtime context and match
  the repository's verified support boundary
- fixed ASDF test-system wiring so raw-checkout verification no longer depends
  on a circular system lookup
- rewired raw-checkout verification scripts to register ASD metadata but load
  repository source and test files directly, avoiding ASDF system-resolution
  hangs on fresh checkouts
- removed duplicate `asdf:test-op` method definitions to keep test runs
  warning-free
- added executable regression tests for the representative README and
  `EXAMPLES.md` workflows
- added `sep-end-by`, `sep-end-by1`, `delimited-sep-end-by`, and
  `delimited-sep-end-by1` for grammars that allow trailing separators
- documented the current public surface in the README
- added contributor and security guidance
- added explicit versioning and release-policy documents for pre-release
  commit consumers
- expanded parser combinator coverage
- fixed parser combinator progress handling so `many` fails fast on
  non-advancing parsers instead of looping
- preserved nested parse failure positions through `map-parser` and
  `bind-parser`
- added `sep-by`, `sep-by1`, and `between` combinators for practical
  delimited-list and bracketed grammar construction
- added `chainl1` and `chainr1` combinators for left- and right-associative
  parser chains without a full Pratt table
- made `diagnostic->string` render source excerpts, notes, and fix-it hints
- fixed Pratt parser EOF failures to return structured diagnostics instead of
  signaling on a missing token span
- added a public Pratt diagnostic example and executable regression coverage
- made `parse-all` preserve trailing tokens in parse failures and attach
  source-backed diagnostics when available
- added a direct token-stream example for `parse-all`
- documented and regression-tested the external-token diagnostic fallback
  contract based on `token-start` / `token-end` plus `(:source <string>)` in
  `token-metadata`
- made `end-of-input` and `not-followed-by` attach source-backed diagnostics
  when the failing token carries span data
- added `preceded-by` and `terminated-by` combinators for delimiter-heavy
  grammars without manual value projection
- updated contributor guidance to treat README, `EXAMPLES.md`, and `examples/`
  workflows as executable public contract
- added roadmap and release-note placeholders for the public OSS surface
- add `ast-node->sexp` and `cst-node->sexp` for stable tree inspection output
- add CST-focused example coverage and regression tests for tree serialization

## Notes

This project does not currently ship formal versioned releases. When releases are
introduced, this file will track user-visible changes in a conventional
`Keep a Changelog` style.
