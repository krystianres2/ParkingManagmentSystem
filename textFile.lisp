(defun split-sequence (delimiter string &key (start 0))
  (loop with len = (length string)
        for pos = start then (1+ end)
        for end = (position delimiter string :start pos)
        collect (subseq string pos (or end len))
        while end))

(defun load-list-from-file (file-path)
  (let ((list '()))
    (with-open-file (file file-path
                          :direction :input
                          :if-does-not-exist :error)
      (loop for line = (read-line file nil)
            while line
            do (let ((auto (make-instance 'VEHICLE)))
                 (destructuring-bind (brand registration colour name surname subscription ile-subscription)
                     (split-sequence #\Space line)
                   (setf (VEHICLE-brand auto) brand)
                   (setf (VEHICLE-registration auto) registration)
                   (setf (VEHICLE-colour auto) colour)
                   (setf (VEHICLE-owner-name auto) name)
                   (setf (VEHICLE-owner-surname auto) surname)
                   (setf (VEHICLE-subscription auto) subscription)
                   (setf (VEHICLE-subscription-time auto) (parse-integer ile-subscription))
                   (setq list (append list (list auto))))))
      list)))  

(defun save-autos-to-file (file-path cars-list)
  (with-open-file (file file-path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (dolist (auto cars-list)
      (format file "~a ~a ~a ~a ~a ~a ~a~%"
              (VEHICLE-brand auto)
              (VEHICLE-registration auto)
              (VEHICLE-colour auto)
              (VEHICLE-owner-name auto)
              (VEHICLE-owner-surname auto)
              (VEHICLE-subscription auto)
              (VEHICLE-subscription-time auto)))))
