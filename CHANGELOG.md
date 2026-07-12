# Changelog

## Unreleased

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
