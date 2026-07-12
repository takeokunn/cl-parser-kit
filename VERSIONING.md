# Versioning

`cl-parser-kit` does not yet publish formal versioned releases.

## Current Consumption Model

Until versioned releases exist:

- consume reviewed commits, not floating branch tips
- pin the exact commit in downstream projects
- rerun `sbcl --script scripts/run-tests.lisp` from that checkout before
  treating it as a supported baseline

## Intended Release Model

When formal releases begin, this repository intends to use semantic versioning
with these expectations:

- patch releases fix incorrect behavior without reshaping the documented
  public contract
- minor releases may add new APIs, examples, and docs
- major releases may remove or redefine public behavior with explicit
  migration notes
