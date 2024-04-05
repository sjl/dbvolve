(in-package :dbvolve)

;;;; Logging -----------------------------------------------------------------
(defparameter *log-stream* *error-output*)

(defun logging (format-string &rest arguments)
  (when *log-stream*
    (apply #'format *log-stream* format-string arguments)))


;;;; Data ---------------------------------------------------------------------
(defclass evolution ()
  ((id :initarg :id :accessor id)
   (name :initarg :name :accessor name)
   (path :initarg :path :accessor path)))

(defclass evolution/sql (evolution)
  ())

(defmethod print-object ((o evolution) s)
  (print-unreadable-object (o s :type t)
    (format s "~D ~A" (id o) (name o))))


;;;; Parsing ------------------------------------------------------------------
(defun evolution-type-p (type)
  (cond ((string-equal "sql" type) :sql)))

(defun parse-evolution-path (path)
  (let ((type (evolution-type-p (pathname-type path))))
    (when type
      (or (let* ((name (pathname-name path))
                 (sep (position-if (lambda (ch) (member ch '(#\- #\_))) name)))
            (when (and sep (plusp sep))
              (let ((id (parse-integer name :end sep :junk-allowed t)))
                (when id
                  (make-instance (ecase type
                                   (:sql 'evolution/sql))
                    :id id
                    :name (subseq name (1+ sep))
                    :path path)))))
          (error "Could not parse evolution filename ~S." path)))))

(defun check-evolutions (evolutions)
  (let ((by-id (make-hash-table)))
    (loop :for e :across evolutions :do (push e (gethash (id e) by-id)))
    (let ((start (id (aref evolutions 0)))
          (end (id (aref evolutions (1- (length evolutions))))))
      (when (not (zerop start))
        (error "Evolution number must start at 0, but first (~S) has ID ~D."
               (path (aref evolutions 0)) start))
      (loop :for id :from start :to end
            :for es = (gethash id by-id)
            :for n = (length es)
            :when (> n 1)
            :do (error "Multiple evolutions found for ID ~D:~{~%  ~S~}"
                       id (mapcar #'path es))
            :when (zerop n)
            :do (error "Gap in evolution numbering, IDs range from ~D to ~D but no evolution found for ~D."
                       start end id)))))

(defun find-evolutions (path)
  (let* ((parsed (remove nil (mapcar #'parse-evolution-path (uiop:directory-files path))))
         (result (sort (coerce parsed 'vector) #'< :key #'id)))
    (if (plusp (length result))
      (check-evolutions result)
      (logging "Could not find any evolutions in ~S." path))
    result))


;;;; Protocol -----------------------------------------------------------------
(defgeneric call-with-new-transaction (db thunk))
(defgeneric create-metadata-table (db))
(defgeneric lock-metadata-table (db))
(defgeneric find-current-number (db))
(defgeneric run-evolution (db evolution))
(defgeneric record-evolution (db evolution))


;;;; Stub Implementation ------------------------------------------------------
(defmethod call-with-new-transaction ((db (eql :fake)) thunk)
  (let ((ok nil))
    (unwind-protect (progn
                      (funcall thunk)
                      (setf ok t))
      (if ok
        (write-line "COMMIT;")
        (write-line "ROLLBACK;")))))

(defmethod create-metadata-table ((db (eql :fake)))
  (write-line (format nil "~
CREATE TABLE IF NOT EXISTS dbvolve (
    id BIGINT PRIMARY KEY,
    name TEXT NOT NULL,
    created TIMESTAMPTZ NOT NULL DEFAULT now()
);")))

(defmethod lock-metadata-table ((db (eql :fake)))
  (write-line "LOCK TABLE dvolve;"))

(defmethod find-current-number ((db (eql :fake)))
  (write-line "SELECT max(id) FROM dbvolve;")
  nil)

(defmethod run-evolution ((db (eql :fake)) (evolution evolution/sql))
  (write-line (uiop:read-file-string (path evolution))))

(defmethod record-evolution ((db (eql :fake)) evolution)
  (write-line (format nil "INSERT INTO dbvolve (id, name, file) VALUES (~S, ~S);"
                      (id evolution)
                      (name evolution))))


;;;; API ----------------------------------------------------------------------
(defun evolve% (database evolutions)
  (map nil (lambda (evolution)
             (logging "    Running  ~A.~%" evolution)
             (run-evolution database evolution)
             (record-evolution database evolution)
             (logging "    Finished ~A.~%" evolution))
       evolutions))

(defun evolve (database evolutions-path)
  (let* ((path (uiop:parse-native-namestring evolutions-path :ensure-directory t))
         (evolutions (find-evolutions path)))
    (when (zerop (length evolutions))
      (logging "No evolutions found in ~S, doing nothing." evolutions-path)
      (return-from evolve))
    (let ((n (length evolutions)))
      (call-with-new-transaction
        database
        (lambda ()
          (logging "Creating metadata table if needed.~%")
          (create-metadata-table database)
          (logging "Obtaining table lock.~%")
          (lock-metadata-table database)
          (let* ((current (or (find-current-number database) -1))
                 (dbn (1+ current))
                 (start (1+ current)))
            (when (> dbn n)
              (logging "Found ~D evolution~:P but DB has ~D, not running anything.~%"
                    n dbn)
              (return-from evolve))
            (logging "Found ~D evolution~:P, DB has ~D, running ~D evolution~:P.~%"
                    n dbn (- n dbn))
            (evolve% database (subseq evolutions start))
            (logging "Finished running ~D evolution~:P successfully.~%" (- n dbn))))))))



