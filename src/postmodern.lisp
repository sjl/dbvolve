(defpackage :dbvolve/postmodern
  (:use :cl :dbvolve/protocol)
  (:export))

(in-package :dbvolve/postmodern)

;;;; Implementation -----------------------------------------------------------
(defmethod dbvolve/protocol:call-with-new-transaction ((db postmodern:database-connection) thunk)
  (let ((postmodern:*database* db))
    (postmodern:with-transaction ()
      (funcall thunk))))

(defmethod dbvolve/protocol:create-metadata-table ((db postmodern:database-connection))
  (let ((postmodern:*database* db))
    (postmodern:execute "CREATE TABLE IF NOT EXISTS dbvolve (
                             id      BIGSERIAL PRIMARY KEY,
                             name    TEXT NOT NULL,
                             created TIMESTAMPTZ NOT NULL DEFAULT now()
                         )")))

(defmethod dbvolve/protocol:lock-metadata-table ((db postmodern:database-connection))
  (let ((postmodern:*database* db))
    (postmodern:execute "LOCK TABLE dbvolve;"))
  (values))

(defmethod dbvolve/protocol:find-current-number ((db postmodern:database-connection))
  (let ((postmodern:*database* db))
    (destructuring-bind ((result))
        (postmodern:query "SELECT max(id) FROM dbvolve;")
      (if (eql :null result)
        -1
        result))))

(defmethod dbvolve/protocol:run-evolution
    ((db postmodern:database-connection) (evolution dbvolve/protocol:evolution/sql))
  (let ((postmodern:*database* db))
    (postmodern:execute-file (dbvolve/protocol:path evolution))))

(defmethod dbvolve/protocol:record-evolution ((db postmodern:database-connection) evolution)
  (let ((postmodern:*database* db))
    (postmodern:execute "INSERT INTO dbvolve (id, name) VALUES ($1, $2);"
                        (dbvolve/protocol:id evolution)
                        (dbvolve/protocol:name evolution))))

