# Changelog

## Unreleased

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
  `*maximum-tokenizer-tokens*`, which signal the exported
  `tokenizer-resource-limit-exceeded` condition instead of exhausting memory on
  an adversarially large or token-dense source
- bounded numeric scanning with `*maximum-number-lexeme-length*` (default 1024)
  so a multi-million-digit run cannot force superlinear bignum work; excess
  digits gracefully start a new token
- bounded diagnostic rendering with `*maximum-diagnostic-line-length*` (default
  400) so a single pathological source line (e.g. a minified file) cannot make
  one `diagnostic->string` call allocate proportional to that line's full length
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
