# Releasing

`cl-parser-kit` does not yet ship formal releases, but the release gate should
already be explicit.

## Release Gate

Before cutting a public release:

1. run `nix flake check` from a clean checkout to execute the full
   reproducible CI gate (compile check, tests, coverage, and lint)
2. run `./scripts/run-release-audit.sh` from the same checkout
3. rerun `sbcl --script scripts/run-compile-check.lisp` to prove both shipped
   ASD systems still compile from a raw checkout
4. rerun `nix develop --command sbcl --script scripts/run-tests.lisp` directly
   if you need a narrower baseline-only confirmation
5. run `sbcl --script scripts/run-examples.lisp` to prove the shipped example
   files still load and produce their documented shapes from a raw checkout
6. run `./scripts/run-implementation-smoke.sh` and record which
   implementations actually passed the compile, test, and example smoke path
   in that environment
7. confirm `README.md`, `API.md`, `EXAMPLES.md`, and `SUPPORT.md` match the
   observed behavior
8. confirm `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md`, `SECURITY.md`,
   `GOVERNANCE.md`, and `MAINTAINERS.md` still describe the active
   contribution, incident, and ownership model
9. confirm `VERSIONING.md` and `ROADMAP.md` still describe the release policy
   and remaining public gaps honestly
10. summarize user-visible changes in `CHANGELOG.md`

## First Release Expectations

The first tagged release should happen only after:

- the repository has a repeatable CI path, not only manual local execution
- the support boundary is still narrow and honestly documented
- the exported API and examples are stable enough to be treated as public
  contract

## After Releases Exist

Once tagged releases begin:

- record every user-visible API or behavior change in `CHANGELOG.md`
- keep migration notes with any breaking change
- update `VERSIONING.md` if the release policy changes
