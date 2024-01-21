(defun split-sequence (delimiter string &key (start 0))
  (loop with len = (length string)
        for pos = start then (1+ end)
        for end = (position delimiter string :start pos)
        collect (subseq string pos (or end len))
        while end))

(defun load-autos-from-file (file-path)
  (let ((lista '()))
    (with-open-file (file file-path
                          :direction :input
                          :if-does-not-exist :error)
      (loop for line = (read-line file nil)
            while line
            do (let ((auto (make-instance 'Auto)))
                 (destructuring-bind (marka rejestracja kolor imie nazwisko abonament ile-abonament)
                     (split-sequence #\Space line)
                   (setf (Auto-marka auto) marka)
                   (setf (Auto-rejestracja auto) rejestracja)
                   (setf (Auto-kolor auto) kolor)
                   (setf (Auto-imieWłaściciela auto) imie)
                   (setf (Auto-nazwiskoWłaściciela auto) nazwisko)
                   (setf (Auto-abonament auto) abonament)
                   (setf (Auto-ileAbonament auto) (parse-integer ile-abonament))
                   (setq lista (append lista (list auto))))))
      (format t "Autos loaded from file.~%")
      lista)))  ; return lista here

(defun save-autos-to-file (file-path lista-aut)
  (with-open-file (file file-path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (dolist (auto lista-aut)
      (format file "~a ~a ~a ~a ~a ~a ~a~%"
              (Auto-marka auto)
              (Auto-rejestracja auto)
              (Auto-kolor auto)
              (Auto-imieWłaściciela auto)
              (Auto-nazwiskoWłaściciela auto)
              (Auto-abonament auto)
              (Auto-ileAbonament auto)))))
