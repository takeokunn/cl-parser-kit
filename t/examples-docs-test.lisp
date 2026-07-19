(in-package :cl-parser-kit/test)

(it-sequential "public-doc-links-resolve-test"
  (dolist (doc-name '("README.md"
                      "API.md"
                      "PARSING_PATTERNS.md"
                      "EXAMPLES.md"
                      "CONTRIBUTING.md"
                      "CODE_OF_CONDUCT.md"
                      "GOVERNANCE.md"
                      "MAINTAINERS.md"
                      "VERSIONING.md"
                      "RELEASING.md"
                      "SUPPORT.md"
                      "SECURITY.md"
                      "ROADMAP.md"
                      "CHANGELOG.md"
                      "ARCHITECTURE.md"))
    (let ((missing '()))
      (dolist (target (markdown-local-links doc-name))
        (unless (probe-file (markdown-link-pathname doc-name target))
          (push target missing)))
      (expect (nreverse missing) :to-equal '()))))

(it-sequential "examples-guide-covers-all-example-files-test"
  (let* ((contents (doc-file-contents "EXAMPLES.md"))
         (missing (loop for name in (example-file-names)
                        unless (search name contents :test #'char-equal)
                        collect name)))
    (expect missing :to-equal '())))

(register-document-snippet-tests
  (readme-documents-verification-and-support-entry-points-test
   "README.md"
   (document-required-snippets "README.md"))
  (security-policy-documents-verified-boundary-test
   "SECURITY.md"
   (document-required-snippets "SECURITY.md"))
  (contributing-guide-documents-verification-contract-test
   "CONTRIBUTING.md"
   (document-required-snippets "CONTRIBUTING.md"))
  (support-guide-documents-example-verification-entry-point-test
   "SUPPORT.md"
   (document-required-snippets "SUPPORT.md"))
  (code-of-conduct-documents-reporting-and-enforcement-test
   "CODE_OF_CONDUCT.md"
   (document-required-snippets "CODE_OF_CONDUCT.md"))
  (governance-documents-maintainer-led-decision-model-test
   "GOVERNANCE.md"
   (document-required-snippets "GOVERNANCE.md"))
  (maintainers-documents-current-ownership-contract-test
   "MAINTAINERS.md"
   (document-required-snippets "MAINTAINERS.md"))
  (versioning-documents-pre-release-consumption-contract-test
   "VERSIONING.md"
   (document-required-snippets "VERSIONING.md"))
  (releasing-documents-current-release-gate-test
   "RELEASING.md"
   (document-required-snippets "RELEASING.md"))
  (roadmap-documents-current-oss-gaps-test
   "ROADMAP.md"
   (document-required-snippets "ROADMAP.md"))
  (api-guide-documents-recommended-entry-points-test
   "API.md"
   (document-required-snippets "API.md/recommended-entry-points")))

(it-sequential "readme-quick-start-surface-matches-api-guide-test"
  (expect (markdown-bullet-code-items "README.md" "### Quick Start Surface") :to-equal (markdown-bullet-code-items "API.md" "## Quick Start Surface")))

(register-document-snippet-tests
  (parsing-patterns-guide-documents-recommended-upgrade-path-test
   "PARSING_PATTERNS.md"
   (document-required-snippets "PARSING_PATTERNS.md"))
  (api-guide-documents-canonical-entry-points-test
   "API.md"
   (document-required-snippets "API.md/canonical-entry-points")))

(it-sequential "api-guide-covers-all-exported-symbols-test"
  (let* ((documented (markdown-code-identifiers "API.md"))
         (missing (sort (loop for symbol being the external-symbols of (find-package :cl-parser-kit)
                              for name = (string-downcase (symbol-name symbol))
                              unless (member name documented :test #'string=)
                              collect name)
                        #'string<)))
    (expect missing :to-equal '())))

(it-sequential "asdf-systems-publish-oss-metadata-test"
  (dolist (name '("cl-parser-kit.asd" "cl-parser-kit-test.asd"))
    (let ((contents (repository-file-contents name)))
      (expect (string-contains-p ":homepage" contents) :to-be-truthy)
      (expect (string-contains-p ":bug-tracker" contents) :to-be-truthy)
      (expect (string-contains-p ":source-control" contents) :to-be-truthy))))

(it-sequential "examples-guide-documents-raw-checkout-example-verification-test"
  (assert-document-contains-all
   "EXAMPLES.md"
   '("scripts/run-examples.lisp"
     "raw-checkout regression pass")))
