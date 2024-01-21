(load "dataValidation.lisp")
(load "textFile.lisp")
(defvar *miejsca* 0)
(defvar *maksimumMiejsc* 20)
(defvar *lista-aut* '()) 

(defclass Auto ()
  ((marka :accessor Auto-marka)
   (rejestracja :accessor Auto-rejestracja)
   (kolor :accessor Auto-kolor)
   (imieWłaściciela :accessor Auto-imieWłaściciela)
   (nazwiskoWłaściciela :accessor Auto-nazwiskoWłaściciela)
   (abonament :accessor Auto-abonament)
   (ileAbonament :accessor Auto-ileAbonament)))


(defun main ()
  (loop
    (format t "1. Dodaj auto~%")
    (format t "2. Usuń auto~%")
    (format t "3. Przedłuż abonament~%")
    (format t "4. Wyświetl listę aut~%")
    (format t "5. Wyjście~%")
    (force-output *query-io*)
    (let ((choice (int-validation 1 5 "Wybierz opcję: ")))
      (cond
        ((= choice 1) (dodajAuto))
        ((= choice 2) (usunAuto))
        ((= choice 3) (przedłużAbonament))
        ((= choice 4) (display-list-info))
        ((= choice 5) (return))
        (t (format t "Niepoprawna opcja~%")))))
)

(defun dodajAuto ()
  (if (< *miejsca* *maksimumMiejsc*)
      (progn
        (setq *miejsca* (incf *miejsca*))
        (let ((*auto* (make-instance 'Auto)))
          (setf (Auto-marka *auto*) (string-validation 2 "Podaj markę auta: "))
          (setf (Auto-rejestracja *auto*) (string-validation 2 "Podaj rejestrację auta: "))
          (setf (Auto-kolor *auto*) (string-validation 2 "Podaj kolor auta: "))
          (setf (Auto-imieWłaściciela *auto*) (string-validation 2 "Podaj imię właściciela auta: "))
          (setf (Auto-nazwiskoWłaściciela *auto*) (string-validation 2 "Podaj nazwisko właściciela auta: "))
          (setf (Auto-ileAbonament *auto*) (int-validation 0 100 "Podaj długość abonamentu auta: "))
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
    (setf pom (int-validation 1 (length *lista-aut*) "Podaj indeks auta, które chcesz usunąć: "))
    (setf *lista-aut* (delete (nth (1- pom) *lista-aut*) *lista-aut*))
          (decf *miejsca*)
    )
  (values)) 

(defun przedłużAbonament ()
  (display-list-compact)
  (let ((pom 0))
    (force-output *query-io*)
    (setf pom (int-validation 1 (length *lista-aut*) "Podaj indeks auta, którego abonament chcesz przedłużyć: "))
    (setf (Auto-ileAbonament (nth (1- pom) *lista-aut*)) (+ (Auto-ileAbonament (nth (1- pom) *lista-aut*)) (int-validation 0 100 "Podaj o ile przedłużyć abonament: ")))
  )
)

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

(defun save-autos-to-file (file-path)
  (with-open-file (file file-path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (dolist (auto *lista-aut*)
      (format file "~a ~a ~a ~a ~a ~a ~a~%"
              (Auto-marka auto)
              (Auto-rejestracja auto)
              (Auto-kolor auto)
              (Auto-imieWłaściciela auto)
              (Auto-nazwiskoWłaściciela auto)
              (Auto-abonament auto)
              (Auto-ileAbonament auto)))))





(load-autos-from-file "autos.txt" *lista-aut*)
(display-list-info)
; (dodajAuto)
; (dodajAuto)

; (save-autos-to-file "autos.txt")

; (main)