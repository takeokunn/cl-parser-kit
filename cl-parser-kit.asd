(asdf:defsystem "cl-parser-kit"
  :description "Small parser toolkit for Common Lisp text languages."
  :version "0.1.0"
  :author "takeokunn"
  :license "MIT"
  :homepage "https://github.com/takeokunn/cl-parser-kit"
  :bug-tracker "https://github.com/takeokunn/cl-parser-kit/issues"
  :source-control (:git "https://github.com/takeokunn/cl-parser-kit.git")
  :pathname "src"
  :serial t
    :components ((:file "package")
                 (:file "core")
                 (:file "spans")
                 (:file "tokens")
                 (:file "tokenizer")
                 (:file "tokenizer-rules")
                 (:file "tokenizer-rules-text")
                 (:file "diagnostics")
                 (:file "diagnostics-format")
                 (:file "tree")
                 (:file "parse-failure")
                 (:file "parse-failure-format")
                 (:file "combinators")
                 (:file "combinators-sequence")
                 (:file "combinators-boundary")
                 (:file "pratt")
                 (:file "pratt-parse")
                 (:file "parser")
                 (:file "ast")
                 (:file "cst")))
