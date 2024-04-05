(defpackage :dbvolve/sqlite
  (:use :cl)
  (:export))

(in-package :dbvolve/sqlite)

;;;; Implementation -----------------------------------------------------------

(defmethod dbvolve/protocol:call-with-new-transaction ((db sqlite:sqlite-handle) thunk)
  (let ((ok nil))
    (sqlite:execute-non-query db "BEGIN TRANSACTION EXCLUSIVE")
    (unwind-protect (progn (funcall thunk)
                           (setf ok t))
      (if ok
        (sqlite:execute-non-query db "COMMIT TRANSACTION")
        (sqlite:execute-non-query db "ROLLBACK TRANSACTION")))))

(defmethod dbvolve/protocol:create-metadata-table ((db sqlite:sqlite-handle))
  (sqlite:execute-non-query db
    "CREATE TABLE IF NOT EXISTS dbvolve (
         id BIGINT PRIMARY KEY,
         name TEXT NOT NULL,
         created TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
     )"))

(defmethod dbvolve/protocol:lock-metadata-table ((db sqlite:sqlite-handle))
  ;; noop due to transaction-based locking
  (values))

(defmethod dbvolve/protocol:find-current-number ((db sqlite:sqlite-handle))
  (or (sqlite:execute-single db "SELECT max(id) FROM dbvolve;") -1))

(defmethod dbvolve/protocol:run-evolution
    ((db sqlite:sqlite-handle) (evolution dbvolve/protocol:evolution/sql))
  (sqlite:execute-script db
    (uiop:read-file-string (dbvolve/protocol:path evolution))))

(defmethod dbvolve/protocol:record-evolution ((db sqlite:sqlite-handle) evolution)
  (sqlite:execute-non-query db
    "INSERT INTO dbvolve (id, name) VALUES (?, ?);"
    (dbvolve/protocol:id evolution) (dbvolve/protocol:name evolution)))

