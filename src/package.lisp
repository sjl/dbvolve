(defpackage :dbvolve/protocol
  (:use :cl)
  (:export
    :call-with-new-transaction
    :create-metadata-table
    :lock-metadata-table
    :find-current-number
    :run-evolution
    :record-evolution

    :evolution
    :evolution/sql
    :id
    :name
    :path))

(defpackage :dbvolve
  (:use :cl :dbvolve/protocol)
  (:export :evolve))
