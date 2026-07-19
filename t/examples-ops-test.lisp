(in-package :cl-parser-kit/test)

(it-sequential "smoke-script-covers-documented-implementation-keys-test"
  (let ((contents (repository-file-contents "scripts/run-implementation-smoke.sh")))
    (assert-string-contains-all
     contents
     '("sbcl" "ccl" "ecl" "clisp" "abcl"
       "scripts/run-compile-check.lisp"
       "scripts/run-tests.lisp"
       "scripts/run-examples.lisp"
       "INFO %s [%s] command:"
       "INFO %s version:"
       "SKIP"
       "PASS"
       "FAIL"))))

(it-sequential "repository-does-not-ship-debug-helper-scripts-test"
  (assert-repository-files-do-not-match
   "scripts/*.lisp"
   (lambda (name)
     (local-string-prefix-p "debug-" name))
   "temporary debug scripts should not ship in the repository: ~S"))

(it-sequential "smoke-script-invokes-checked-in-verification-entrypoints-directly-test"
  (let ((script (repository-file-contents "scripts/run-implementation-smoke.sh")))
    (dolist (needle '("compile_check_script="
                      "run_tests_script="
                      "run_examples_script="
                      "scripts/run-compile-check.lisp"
                      "scripts/run-tests.lisp"
                      "scripts/run-examples.lisp"
                      "run_check"
                      "run_check \"$impl_name\" compile"
                      "run_check \"$impl_name\" tests"
                      "run_check \"$impl_name\" examples"
                      "impl_version"
                      "join_command"))
      (expect (search needle script) :to-be-truthy))
    (assert-string-lacks-any
     script
     '("driver_file=" "cat >\"$driver_file\"" "trap cleanup")
     "Smoke script should no longer depend on temporary driver artifact ~S.")))

(it-sequential "release-audit-script-enforces-release-gate-contract-test"
  (let ((script (repository-file-contents "scripts/run-release-audit.sh")))
    (dolist (needle '("LICENSE"
                      "README.md"
                      "API.md"
                      "EXAMPLES.md"
                      "ARCHITECTURE.md"
                      "PARSING_PATTERNS.md"
                      "GOVERNANCE.md"
                      "MAINTAINERS.md"
                      "VERSIONING.md"
                      "CONTRIBUTING.md"
                      "CODE_OF_CONDUCT.md"
                      "SUPPORT.md"
                      "SECURITY.md"
                      "RELEASING.md"
                      "ROADMAP.md"
                      "CHANGELOG.md"
                      "scripts/run-tests.lisp"
                      "scripts/run-implementation-smoke.sh"
                      "README points contributors at the contributing guide"
                      "CONTRIBUTING references security policy"
                      "SUPPORT references the release policy"
                      "RELEASING includes the security policy in the gate"
                      "RELEASING includes the governance policy in the gate"
                      "RELEASING includes the maintainer policy in the gate"
                      "RELEASING includes the versioning policy in the gate"
                      "GOVERNANCE requires executable evidence for behavioral claims"
                      "MAINTAINERS preserves the raw-checkout verification baseline"
                      "ROADMAP records the reproducible CI verification path"
                      "run_with_timeout"
                      "run_step_with_timeout"
                      "timeout 300"
                      "README quick-start API bullets mirror API.md"
                      "sbcl --script"
                      "PASS release readiness audit"))
      (expect (search needle script) :to-be-truthy))))
