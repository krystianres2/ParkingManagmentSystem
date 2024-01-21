(defun int-validation (min max tekst)
  (let ((input 0) (valid nil))
    (loop until (equal valid T) do
      (format t "~A~%" tekst)
      (force-output *query-io*)
      (setf input (read *query-io*))
      (if (and (integerp input) (<= min input max))
          (setq valid T)
          (format t "Wartość musi być liczbą z zakresu ~a i ~a.~%" min max)))
    input))


(defun string-validation (minlength tekst)
  (let ((input nil) (valid nil))
    (loop until (equal valid T) do
      (format t "~A~%" tekst)
      (force-output *query-io*)
      (setf input (read-line *query-io*))
      (if (and (stringp input)
               (>= (length input) minlength)
               (not (integerp (ignore-errors (parse-integer input)))))
          (setq valid T)
          (format t "Wartość musi być niepustym napisem o co najmniej ~a znakach i nie może być liczbą.~%" minlength)))
    input))

