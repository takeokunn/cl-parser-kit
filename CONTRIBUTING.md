# Contributing

`cl-parser-kit` is intentionally small. Contributions should preserve that
shape.

## Before You Change Code

- read [`ARCHITECTURE.md`](./ARCHITECTURE.md) for the layer model and dependency direction
- check the existing tests and examples
- keep exported symbols intentional
- prefer the smallest change that solves the problem

## Expectations

- add or update tests for new behavior
- keep the README in sync with public API changes
- keep `API.md` / `EXAMPLES.md` in sync when public entry points or contracts change
- keep `SUPPORT.md` in sync when verification scope or support claims change
- keep `SECURITY.md` in sync when reporting paths or security-scoped claims change
- keep `GOVERNANCE.md` / `MAINTAINERS.md` in sync when review or ownership
  expectations change
- keep `CODE_OF_CONDUCT.md` in sync when collaboration expectations or
  enforcement guidance change
- keep `VERSIONING.md` / `RELEASING.md` in sync when release expectations or
  public API text changes
- avoid introducing broad utility layers or hidden dependencies
- keep changes readable from the test suite
- preserve the executable example contract: representative README snippets and
  files under `examples/` are regression-tested and should keep working from a
  fresh image

## Working Style

- use `nix flake check` to run the full reproducible gate (compile check,
  tests, coverage, and lint) the same way CI does, on `x86_64-linux` or
  `aarch64-linux`
- use `nix develop --command sbcl --script scripts/run-tests.lisp` to rerun
  only the test suite with its pinned `cl-weave` and `cl-prolog` dependencies
- use `sbcl --script scripts/run-compile-check.lisp` when a change could
  affect ASDF loading, package wiring, or compile-time behavior
- use `sbcl --script scripts/run-examples.lisp` when a change touches public
  sample workflows or documented result shapes
- use `./scripts/run-implementation-smoke.sh` when a change is specifically
  about implementation portability or support-boundary claims
- if you already keep the repository on ASDF's search path, `asdf:test-system
  :cl-parser-kit` remains the equivalent REPL-level entry point
- when behavior changes affect diagnostics, examples, or public exports,
  prefer the narrowest supporting regression test in `t/` plus the full ASDF
  suite before handing work off
- keep parser, tokenizer, and diagnostic behavior explicit
- prefer simple data structures over extra abstraction

## Release Checklist

Before proposing a user-visible change, verify:

- tests for the changed behavior exist or were updated
- public docs (`README.md`, `API.md`, `EXAMPLES.md`) match the shipped API
- `SUPPORT.md` matches the verification reality of the current checkout
- collaboration docs (`CODE_OF_CONDUCT.md`, `GOVERNANCE.md`,
  `MAINTAINERS.md`) still describe the actual maintenance model
- `SECURITY.md` still points reporters at the right private contact and
  verified support boundary
- `VERSIONING.md` and `RELEASING.md` still describe the actual release policy
- example files still load and return the documented shape
- `sbcl --script scripts/run-compile-check.lisp` passes when system wiring,
  package setup, or compile-time behavior moved
- `sbcl --script scripts/run-examples.lisp` passes when examples or docs moved
- `./scripts/run-implementation-smoke.sh` was rerun when portability-facing
  behavior or documented contract changed
- `./scripts/run-release-audit.sh` still passes from the checkout you modified
- `nix flake check` passes from the checkout you modified

## Reporting Bugs

When filing a bug report, include:

- the Lisp implementation and version
- the exact input that failed
- the observed result
- the expected result
