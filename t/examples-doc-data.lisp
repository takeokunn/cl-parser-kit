(in-package :cl-parser-kit/test)

(defparameter *dsl-sample-source*
  "if $value /* note */ \"ok\" ; trailing comment
if?")

(defparameter *diagnostic-sample-source* "foo + bar")

(defun document-required-snippets (document)
  (cond
    ((string= document "README.md")
     '("scripts/run-tests.lisp"
       "scripts/run-compile-check.lisp"
       "scripts/run-examples.lisp"
       "scripts/run-release-audit.sh"
       "scripts/run-implementation-smoke.sh"
       "SUPPORT.md"
       "PARSING_PATTERNS.md"
       "SECURITY.md"
       "CONTRIBUTING.md"
       "CODE_OF_CONDUCT.md"
       "does not yet ship formal versioned releases"))
    ((string= document "SECURITY.md")
     '("scripts/run-compile-check.lisp"
       "nix flake check"
       "scripts/run-examples.lisp"
       "./scripts/run-implementation-smoke.sh"
       "SUPPORT.md"
       "Common Lisp implementation and version"
       "private reporting"))
    ((string= document "CONTRIBUTING.md")
     '("scripts/run-tests.lisp"
       "scripts/run-compile-check.lisp"
       "scripts/run-examples.lisp"
       "nix flake check"
       "API.md"
       "EXAMPLES.md"
       "SUPPORT.md"
       "SECURITY.md"
       "CODE_OF_CONDUCT.md"
       "GOVERNANCE.md"
       "MAINTAINERS.md"
       "Release Checklist"
       "./scripts/run-release-audit.sh"
       "example files still load and return the documented shape"))
    ((string= document "SUPPORT.md")
     '("scripts/run-compile-check.lisp"
       "scripts/run-examples.lisp"
       "sample files"))
    ((string= document "CODE_OF_CONDUCT.md")
     '("SECURITY.md"
       "conduct concern"
       "Repeated or severe violations"
       "Direct, rigorous review"))
    ((string= document "GOVERNANCE.md")
     '("maintainer-led model"
       "behavioral claims are expected to be backed by executable tests"
       "keep the public surface small and intentional"
       "does not currently use formal voting"))
    ((string= document "MAINTAINERS.md")
     '("README.md"
       "API.md"
       "nix flake check"
       "sbcl --script scripts/run-tests.lisp"
       "no guaranteed response-time SLA"))
    ((string= document "VERSIONING.md")
     '("does not yet publish formal versioned releases"
       "pin the exact commit"
       "semantic versioning"))
    ((string= document "RELEASING.md")
     '("run `nix flake check`"
       "run `./scripts/run-release-audit.sh`"
       "run `sbcl --script scripts/run-compile-check.lisp`"
       "nix develop --command sbcl --script scripts/run-tests.lisp"
       "run `sbcl --script scripts/run-examples.lisp`"
       "run `./scripts/run-implementation-smoke.sh`"
       "CONTRIBUTING.md"
       "CODE_OF_CONDUCT.md"
       "SECURITY.md"
       "README.md"
       "CHANGELOG.md"
       "ROADMAP.md"
       "repeatable CI path"))
    ((string= document "ROADMAP.md")
     '("repository-level `nix flake check` CI"
       "coverage"
       "cut the first tagged release"
       "first tagged release"
       "portability-sensitive parser and"
       "release notes for tagged releases"
       "public surface small and intentional"))
    ((string= document "API.md/recommended-entry-points")
     '("## Recommended Entry Points"
       "## Quick Start Surface"
       "PARSING_PATTERNS.md"
       "tokenize source with `make-tokenizer` and `tokenize-string`"
       "For comma-separated or bracketed forms, start with `sep-by`, `sep-by1`"
       "start with `chainl1` or `chainr1`; pair them with `operator-parser`"
       "use `make-ast-node` or `make-cst-node` to shape downstream data"
       "use `ast-node->sexp` or `cst-node->sexp`"))
    ((string= document "PARSING_PATTERNS.md")
     '("## Start With The Smallest Stable Layer"
       "Prefer Sequence Helpers Over Manual Delimiter Loops"
       "Use `delimited-sep-by` or `delimited-sep-by1`"
       "`sep-end-by` / `sep-end-by1` keep that same committed-item rule"
       "Use `type-token-text` or `type-token-value`"
       "alt` returns the farthest branch failure"
       "Move to Pratt parsing when you need:"
       "Replace hand-written delimiter plumbing with `preceded-by`"))
    ((string= document "API.md/canonical-entry-points")
     '("## Parser Entry Points"
       "`parse-tokens`"
       "`parse-all`"
       "`parse-source`"
       "`parse-pratt`"
       "`parse-pratt-source`"
       "End-to-end entry points intentionally stay small"))
    (t
     (error "Unknown document snippet contract: ~S" document))))
