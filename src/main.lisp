(in-package :dbvolve)

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
      (warn "Could not find any evolutions in ~S." path))
    result))


;;;; Protocol -----------------------------------------------------------------
(defgeneric call-with-new-transaction (db thunk))
(defgeneric create-metadata-table (db))
(defgeneric lock-metadata-table (db))
(defgeneric find-current-number (db))
(defgeneric dump-current-state (db))
(defgeneric run-evolution (db evolution))
(defgeneric record-evolution (db evolution))
(defgeneric commit (db))


;;;; Stub Implementation ------------------------------------------------------
(defmethod call-with-new-transaction ((db null) thunk)
  (funcall thunk))

(defmethod create-metadata-table ((db null))
  (write-line "
    CREATE TABLE IF NOT EXISTS dbvolve (
      id BIGINT PRIMARY KEY,
      name TEXT NOT NULL,
      created TIMESTAMPTZ NOT NULL DEFAULT now()
    );
    "))

(defmethod lock-metadata-table ((db null))
  (write-line "LOCK TABLE dvolve;")
  )

(defmethod find-current-number ((db null))
  (write-line "SELECT max(id) FROM dbvolve;")
  1)

(defmethod dump-current-state ((db null))
  (write-line "SELECT * FROM dbvolve;"))

(defmethod run-evolution ((db null) (evolution evolution/sql))
  (write-line (uiop:read-file-string (path evolution))))

(defmethod record-evolution ((db null) evolution)
  (write-line (format nil "INSERT INTO dbvolve (id, name, file) VALUES (~S, ~S);"
                      (id evolution)
                      (name evolution))))

(defmethod commit ((db null)))


;;;; API ----------------------------------------------------------------------
(defun evolve% (database evolutions)
  (dolist (evolution evolutions)
    (format t "~%Running ~A.~%" evolution)
    (run-evolution database evolution)
    (record-evolution database evolution)
    (format t "Finished ~A.~%" evolution)))

(defun evolve (database evolutions-path)
  (let* ((path (uiop:parse-native-namestring evolutions-path :ensure-directory t))
         (evolutions (find-evolutions path)))
    (if (zerop (length evolutions))
      (warn "No evolutions found in ~S, doing nothing." evolutions-path)
      (let ((n (length evolutions)))
        (call-with-new-transaction
          database
          (lambda ()
            (create-metadata-table database)
            (lock-metadata-table database)
            (let* ((current (find-current-number database))
                   (start (1+ current)))
              (format t "Found ~D evolution~:P, DB has ~D, running ~D evolution~:P.~%"
                      n (1+ current) (- n start))
              (evolve% database (subseq evolutions start))
              (commit)
              (format t "Finished running ~D evolution~:P successfully.~%" n))))))))



