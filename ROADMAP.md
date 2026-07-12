# Roadmap

`cl-parser-kit` is intentionally small. The next steps focus on keeping the API
practical, testable, and easy to audit.

## Near Term

- add repository-level CI for `asdf:test-system :cl-parser-kit` so verification
  does not depend on manual local execution
- cut the first tagged release only after the verification path is repeatable
  and the public boundary is documented tightly enough to audit
- keep adding targeted regression tests for portability-sensitive parser and
  diagnostic paths

## Mid Term

- add release notes for tagged releases once versioned releases exist
- tighten the documentation around recommended parser composition and upgrade
  patterns as the public API settles
- keep the public surface small and intentional

## Non-Goals

- compiler framework features
- editor integration
- CLI/runtime scaffolding
- large opinionated abstractions over the parser core
