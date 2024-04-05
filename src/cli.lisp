(defpackage :dbvolve/cli
  (:use :cl)
  (:export :toplevel :build))

(in-package :dbvolve/cli)


;;;; Configuration ------------------------------------------------------------
(defparameter *version* "0.0.1")


;;;; Run ----------------------------------------------------------------------
(defun database (options)
  (ecase (gethash 'database options)
    (:sqlite (sqlite:connect (or (gethash 'dbi-path options)
                                 (error "--db-path is required for sqlite"))))
    (:postgres (postmodern:connect
                 (or (gethash 'dbi-database options)
                     (error "--db-database is required for Postgres"))
                 (or (gethash 'dbi-user options)
                     (error "--db-user is required for Postgres"))
                 (or (gethash 'dbi-password options)
                     (error "--db-password is required for Postgres"))
                 (or (gethash 'dbi-host options)
                     (error "--db-host is required for Postgres"))
                 :port (or (gethash 'dbi-port options)
                           (error "--db-port is required for Postgres"))))
    (:fake :fake)))

(defun run (evolutions-path options)
  (let ((db (database options)))
    (dbvolve:evolve db evolutions-path)))


;;;; User Interface -----------------------------------------------------------
(defparameter *examples*
  '(("Evolve a sqlite3 database:"  . "dbvolve path/to/evolutions/ --sqlite --db-path foo.sqlite")
    ("Evolve a Postgres database:" . "dbvolve path/to/evolutions/ --postgres --db-user user --db-host localhost --db-port 5432 --db-database testdb --db-password-command 'retrieve-secret.sh testdb-password'")))


(defparameter *option-help*
  (adopt:make-option 'help
    :help "Display help and exit."
    :long "help"
    :short #\h
    :reduce (constantly t)))

(defparameter *option-version*
  (adopt:make-option 'version
    :help "Display version information and exit."
    :long "version"
    :reduce (constantly t)))

(defparameter *option-db-sqlite*
  (adopt:make-option 'sqlite
    :result-key 'database
    :help "Connect to a sqlite3 database at PATH.  Requires --db-path."
    :long "sqlite"
    :reduce (constantly :sqlite)))

(defparameter *option-db-postgres*
  (adopt:make-option 'postgres
    :result-key 'database
    :help "Connect to a Postgres database.  Requires --db-user, etc."
    :long "postgres"
    :reduce (constantly :postgres)))

(defparameter *option-db-fake*
  (adopt:make-option 'fake
    :result-key 'database
    :help "Connect to a fake database and dump SQL to standard output."
    :long "fake"
    :reduce (constantly :fake)))

(defparameter *option-dbi-path*
  (adopt:make-option 'dbi-path
    :help "Connect to database file at PATH."
    :parameter "PATH"
    :long "db-path"
    :reduce #'adopt:last))

(defparameter *option-dbi-host*
  (adopt:make-option 'dbi-host
    :help "Connect to database running at HOST."
    :parameter "HOST"
    :long "db-host"
    :reduce #'adopt:last))

(defparameter *option-dbi-user*
  (adopt:make-option 'dbi-user
    :help "Connect to database as USER."
    :parameter "USER"
    :long "db-user"
    :reduce #'adopt:last))

(defparameter *option-dbi-password*
  (adopt:make-option 'dbi-password
    :result-key 'dbi-password
    :help "Connect to database with password PASS."
    :parameter "PASS"
    :long "db-password"
    :reduce #'adopt:last))

(defparameter *option-dbi-password-file*
  (adopt:make-option 'dbi-password-file
    :result-key 'dbi-password
    :help "Connect to database with password read from PATH."
    :parameter "PATH"
    :long "db-password-file"
    :key (lambda (path)
           (string-trim '(#\newline) (uiop:read-file-string path)))
    :reduce #'adopt:last))

(defparameter *option-dbi-password-command*
  (adopt:make-option 'dbi-password
    :result-key 'dbi-password
    :help "Connect to database with password output on stdout from CMD."
    :parameter "CMD"
    :long "db-password-command"
    :key (lambda (cmd)
           (uiop:run-program cmd :output '(:string :stripped t)))
    :reduce #'adopt:last))

(defparameter *option-dbi-port*
  (adopt:make-option 'dbi-port
    :help "Connect to database on port PORT."
    :parameter "PORT"
    :long "db-port"
    :key #'parse-integer
    :reduce #'adopt:last))

(defparameter *option-dbi-database*
  (adopt:make-option 'dbi-database
    :help "Connect to database named DB."
    :parameter "DB"
    :long "db-database"
    :reduce #'adopt:last))

(defparameter *group-databases*
  (adopt:make-group 'databases
    :title "Databases"
    :options (list *option-db-sqlite*
                   *option-db-postgres*
                   *option-db-fake*)))

(defparameter *group-database-info*
  (adopt:make-group 'database-info
    :title "Database Info"
    :options (list
               *option-dbi-path*
               *option-dbi-database*
               *option-dbi-host*
               *option-dbi-port*
               *option-dbi-user*
               *option-dbi-password*
               *option-dbi-password-file*
               *option-dbi-password-command*)))


(adopt:define-string *help-text*
  "Command line interface to the Common Lisp DBvolve library.")

(defparameter *ui*
  (adopt:make-interface
    :name "dbvolve"
    :usage "[OPTIONS] PATH"
    :summary "evolve a database"
    :help *help-text*
    :examples *examples*
    :contents (list *option-help*
                    *option-version*
                    *group-databases*
                    *group-database-info*)))

(defun toplevel ()
  (handler-case
      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
        (when (gethash 'help options)
          (adopt:print-help-and-exit *ui*))
        (when (gethash 'version options)
          (write-line *version*)
          (adopt:exit))
        (cond ((null arguments)
               (error "PATH to evolutions directory is required."))
              ((> (length arguments) 1)
               (error "Exactly one PATH to evolutions directory is required, got: ~S" arguments)))
        (let ((db (gethash 'database options))
              (evolutions (first arguments)))
          (when (null db)
            (error "No database specified."))
          (run evolutions options)))
    (error (c) (adopt:print-error-and-exit c))))


(defun build ()
  (with-open-file (f "dbvolve.1" :direction :output :if-exists :supersede)
    (adopt:print-manual *ui* :stream f))
  (sb-ext:save-lisp-and-die "dbvolve"
    :executable t
    :compression t
    :save-runtime-options t
    :toplevel 'toplevel))
