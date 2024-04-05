(asdf:defsystem :dbvolve
  :description "Database schema evolution."
  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://docs.stevelosh.com/dbvolve/"

  :license "MIT"
  :version "0.0.1"

  :depends-on (:uiop)

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
