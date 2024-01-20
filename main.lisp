(defvar *miejsca* 0)
(defvar *maksimumMiejsc* 20)
(defvar *lista-aut* '()) 

(defun dodajAuto ()
  (if (< *miejsca* *maksimumMiejsc*)
      (progn
        (setq *miejsca* (incf *miejsca*))
        (let ((*auto* (make-instance 'Auto)))
          (print "Podaj markę auta: ")
          (force-output *query-io*)
          (setf (Auto-marka *auto*) (read *query-io*))

          (print "Podaj rejestrację auta: ")
          (force-output *query-io*)
          (setf (Auto-rejestracja *auto*) (read *query-io*))

          (print "Podaj kolor auta: ")
          (force-output *query-io*)
          (setf (Auto-kolor *auto*) (read *query-io*))

          (print "Podaj imię właściciela auta: ")
          (force-output *query-io*)
          (setf (Auto-imieWłaściciela *auto*) (read *query-io*))

          (print "Podaj nazwisko właściciela auta: ")
          (force-output *query-io*)
          (setf (Auto-nazwiskoWłaściciela *auto*) (read *query-io*))

          (print "Podaj długość abonamentu auta: ")
          (force-output *query-io*)
          (setf (Auto-ileAbonament *auto*) (read *query-io*))
          (if (> (Auto-ileAbonament *auto*) 0)
              (setf (Auto-abonament *auto*) T)
              (setf (Auto-abonament *auto*) NIL))

          (setq *lista-aut* (append *lista-aut* (list *auto*)))
          ))
    (print "Brak wolnych miejsc na parkingu")))

(defun usunAuto ()
  (display-list-compact)
  
  (let ((pom 0))
    (print "Podaj indeks auta, które chcesz usunąć: ")
    (force-output *query-io*)
    (setf pom (read *query-io*))
    
    (if (and (>= pom 1) (<= pom (length *lista-aut*)))
        (progn
          (setf *lista-aut* (delete (nth (1- pom) *lista-aut*) *lista-aut*))
          (decf *miejsca*))
        (print "Nieprawidłowy indeks auta")))
  
  (values)) 

; (defun przedłużAbonament (auto Auto)
;   (display-list-compact)

;   (let ((pom 0))
;     (print "Podaj indeks auta, którego abonament chcesz przedłużyć: ")
;     (force-output *query-io*)
;     (setf pom (read *query-io*))
    
;     (if (not (null (nth (- pom 1) *lista-aut*)))
;     (progn
;       (let ((pom2 0))
;         (print "O ile godzin chcesz przedłużyć abonament: ")
;         (force-output *query-io*)
;         (setf pom2 (read *query-io*))

        
;       )
;     )
;     )
;    )

; )


(defun print-auto-details (auto)
  (format t "Marka: ~a~%" (Auto-marka auto))
  (format t "Rejestracja: ~a~%" (Auto-rejestracja auto))
  (format t "Kolor: ~a~%" (Auto-kolor auto))
  (format t "Imię właściciela: ~a~%" (Auto-imieWłaściciela auto))
  (format t "Nazwisko właściciela: ~a~%" (Auto-nazwiskoWłaściciela auto))
  (format t "Abonament: ~a~%" (Auto-abonament auto))
  (format t "Ile abonament: ~a~%" (Auto-ileAbonament auto)))

(defun display-list-info ()
  (format t "Liczba dostępnych miejsc na parkingu: ~a~%" (- *maksimumMiejsc* *miejsca*))
  (format t "Zawartość listy aut:~%")

  (dolist (auto *lista-aut*)
    (format t "----------------------------~%")
    (print-auto-details auto))

  (format t "----------------------------~%"))

(defun display-list-compact ()
  (format t "Liczba dostępnych miejsc na parkingu: ~a~%" (- *maksimumMiejsc* *miejsca*))
  (format t "Zawartość listy aut:~%")

  (loop for (auto index) in (loop for auto in *lista-aut* for i from 1 collect (list auto i))
    do (format t "~d. Marka: ~a, Rejestracja: ~a, Kolor: ~a, Właściciel: ~a ~a, Abonament: ~a, Ile abonament: ~a~%"
                index
                (Auto-marka auto)
                (Auto-rejestracja auto)
                (Auto-kolor auto)
                (Auto-imieWłaściciela auto)
                (Auto-nazwiskoWłaściciela auto)
                (if (Auto-abonament auto) "Tak" "Nie")
                (Auto-ileAbonament auto)))
  (format t "----------------------------~%"))

(defclass Auto ()
  ((marka :accessor Auto-marka)
   (rejestracja :accessor Auto-rejestracja)
   (kolor :accessor Auto-kolor)
   (imieWłaściciela :accessor Auto-imieWłaściciela)
   (nazwiskoWłaściciela :accessor Auto-nazwiskoWłaściciela)
   (abonament :accessor Auto-abonament)
   (ileAbonament :accessor Auto-ileAbonament)))


(dodajAuto)
(dodajAuto)
(usunAuto)

(display-list-info)