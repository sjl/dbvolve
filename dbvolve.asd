(asdf:defsystem :dbvolve
  :description "Database schema evolution."
  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://docs.stevelosh.com/dbvolve/"

  :license "MIT"
  :version "0.0.1"

  :depends-on (:uiop)

  :in-order-to ((asdf:test-op (asdf:test-op :dbvolve/test)))

  :serial t
  :components ((:module "src" :serial t
                :components ((:file "package")
                             (:file "main")))))

(asdf:defsystem :dbvolve/postmodern
  :description "DBvolve for Postmodern."
  :author "Steve Losh <steve@stevelosh.com>"

  :depends-on (:dbvolve :postmodern)

  :serial t
  :components ((:module "src" :serial t
                :components ((:file "postmodern")))))

(asdf:defsystem :dbvolve/sqlite
  :description "DBvolve for cl-sqlite."
  :author "Steve Losh <steve@stevelosh.com>"

  :depends-on (:dbvolve :sqlite)

  :serial t
  :components ((:module "src" :serial t
                :components ((:file "sqlite")))))

(asdf:defsystem :dbvolve/cli
  :description "CLI program for DBvolve."
  :author "Steve Losh <steve@stevelosh.com>"

  :depends-on (:dbvolve :dbvolve/sqlite :dbvolve/postmodern :adopt)

  :serial t
  :components ((:module "src" :serial t
                :components ((:file "cli")))))


(asdf:defsystem :dbvolve/test
  :description "Test suite for DBvolve."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (:dbvolve/sqlite :1am)

  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))

  :perform (asdf:test-op (op system)
             (funcall (read-from-string "dbvolve/test:run-tests"))))
