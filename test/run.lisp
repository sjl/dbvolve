#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload :dbvolve :silent t)
(asdf:test-system :dbvolve)
(quit)
