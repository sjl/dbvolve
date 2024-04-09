(in-package :dbvolve/test)


;;;; Utils --------------------------------------------------------------------
(defmacro define-test (name &body body)
  `(test ,(intern (concatenate 'string (symbol-name 'test/) (symbol-name name)))
    (let ((*package* ,*package*))
      ,@body)))

(defmacro with-db (db-symbol &body body)
  `(sqlite:with-open-database (,db-symbol ":memory:")
     (let ((dbvolve::*log-stream* (make-broadcast-stream)))
       ,@body)))

(defun run-tests ()
  (1am:run))

(defmacro check-migrations (db &rest expected)
  `(is (equal '(,@expected)
              (sqlite:execute-to-list ,db "select id, name from dbvolve order by id;"))))

;;;; Tests --------------------------------------------------------------------
(define-test no-migrations
  (with-db db
    ;; Should not signal an error.
    (dbvolve:evolve db "test/example-0/")
    (check-migrations db)))

(define-test single-migration
  (with-db db
    (dbvolve:evolve db "test/example-1/")
    (check-migrations db (0 "users-table"))
    (is (equal '((0 "sjl"))
               (sqlite:execute-to-list db "select * from users;")))))

(define-test one-at-a-time
  (with-db db
    (dbvolve:evolve db "test/example-1/")
    (check-migrations db (0 "users-table"))
    (is (equal '((0 "sjl"))
               (sqlite:execute-to-list db "select * from users;")))
    (dbvolve:evolve db "test/example-2/")
    (check-migrations db (0 "users-table") (1 "add-email"))
    (is (equal '((0 "sjl" "steve@stevelosh.com"))
               (sqlite:execute-to-list db "select * from users;")))))

(define-test multiple-migrations
  (with-db db
    (dbvolve:evolve db "test/example-3/")
    (is (equal '((0 0 "Write DBvolve skeleton." 1)
                 (1 0 "Write DBvolve test suite." 0))
               (sqlite:execute-to-list db "select id, user_id, content, done from todos;")))
    (check-migrations db (0 "users-table") (1 "add-email") (2 "add-todos"))))

(define-test broken-migrations
  (with-db db
    ;; start with an empty set
    (dbvolve:evolve db "test/example-0/")
    (check-migrations db)
    (dbvolve:evolve db "test/broken-1/")
    (check-migrations db (0 "table"))
    (signals error (dbvolve:evolve db "test/broken-2/"))
    (check-migrations db (0 "table")) ; should not have applied bad migration
    (signals error (dbvolve:evolve db "test/broken-3/")) ; broken migrations stops all further ones
    (check-migrations db (0 "table")))
  (with-db db
    ;; start with an empty set
    (dbvolve:evolve db "test/example-0/")
    (check-migrations db)
    ;; should be all-or-nothing
    (signals error (dbvolve:evolve db "test/broken-3/"))
    (check-migrations db)))

(define-test bad-name
  (with-db db
    (signals error (dbvolve:evolve db "test/bad-name-1/"))
    (signals error (dbvolve:evolve db "test/bad-name-2/"))
    (signals error (dbvolve:evolve db "test/bad-name-3/"))))
