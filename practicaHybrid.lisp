(defun unificar-init (entrada1 entrada2)
  (let ((theta nil))  ;Iniciamos el unificador a null
    (if (equalp (first entrada1) (first entrada2))
      (unificar (rest entrada1) (rest entrada2) theta)
      'error 
    ) 
  ) 
)

(defun miembro (entrada lista) ;Devuelve T si entrada se encuentra en la lista
  (cond 
    ((equalp entrada lista) t)
    ((atomp lista) nil)
    (t (or 
          (miembro entrada (first lista))
          (miembro entrada (rest lista)) 
        )
    ) 
  ) 
)

(defun unificar (e1 e2 theta)
  (cond
    ((or (equalp e1 'error) (equalp e2 'error))
      'error
    )
    ((atomp e1)
      (top e1 e2 theta)
    )
    ((atomp e2)
      (top e2 e1 theta)
    )
    (t (setf theta (unificar (aplicarsustitucion (first e1) theta)
                       (aplicarsustitucion (first e2) theta) 
                       theta) )
        (if (equalp theta 'error)
          'error
        )
       (unificar (rest e1) (rest e2) theta) 
    )
  ) 
)

(defun top (e1 e2 theta)
  (cond
    ((equalp e1 e2) theta)
    ((or (null e1) (null e2))
      'error
    )
    ((variablep e1) ;si e1 es variable
      (if (miembro e1 e2)
        'error ; return error si es miembro
        (anadir e2 e1 theta)
      )
    )
    ((variablep e2)
      (anadir e1 e2 theta)
    )
    (t 'error)   
  )
)

(defun atomp (s)
    (cond((atom s) T)
    ((eq (first s) '?) T)
    (t NIL)
  )
)

(defun anadir (termino variable theta)
  (cons (list termino variable) 
    (sustituirexpresion termino variable theta)   
  )
)

(defun aplicarsustitucion (expresion lista)  
  (cond 
    ((null lista) expresion)
    ((equalp lista 'error)
      'error
    )
    (t 
      (sustituirexpresion (first (first lista)) 
        (second (first lista))
        (aplicarsustitucion expresion (rest lista)) 
      )
    ) 
  ) 
)

(defun sustituirexpresion (a b lista)
  (cond
    ((null lista) nil)
    ((equalp lista b) a)
    ((atomp lista) lista)
    ((equalp b (first lista))
    (cons a (sustituirexpresion a b (rest lista))) )
    ((atomp (first lista))
    (cons (first lista)(sustituirexpresion a b (rest lista))) )
    (t (cons (sustituirexpresion a b (first lista))
      (sustituirexpresion a b (rest lista)) 
      )
    ) 
  ) 
)

(defun variablep (x)
  (if (atom x) nil ;si es atomo devolver nil
    (if ;si no lo es,
      (and
        (eq (first x) '?)
        (eq (length x) 2)
      ) t  ;y es una lista de len 2 con ? de primer argumento, ES UNA VARIABLE
      nil ;en cualquier otro caso, NO ES VARIABLE
    )
  )
)

;Casos de prueba

(defparameter e1 '(p (f a (? x)) (? z)))
(defparameter e2 '(p (? y) ((? w) d)))



;;; Here's a function for demonstrating UNIFY.
(defun test-unificar (e1 e2)
  (format t "~%(unificar ~s ~s): ~s."
    e1 e2 (unificar-init e1 e2)
  )
)

(defun test ()
  (test-unificar e1 e2)
  (let ((theta (unificar-init e1 e2)))
    (format t "~%Aplicando theta a ~s obtenemos ~s."
            e1 (aplicarsustitucion e1 theta) )
    (format t "~%Aplicando theta a ~s obtenemos ~s."
            e2 (aplicarsustitucion e2 theta) )
  ) 
  (test-aplicarsustitucion)
  (test-miembro)
  (test-anadir)
  (test-sustituirexpresion)
)

(defun test-aplicarsustitucion ()
  (setf s1 '( (a (? x)) ((? y) (? z)) ((f (h)) k) ))
  (setf p1 '( (? x) g (k) f2 ((? z))))
  (format t "~%Resultado de aplicar la sustitucion ~s a la lista ~s es: ~s"
    s1 p1 (aplicarsustitucion p1 s1))
)

(defun test-miembro()
  (setf s2 '( (a (? x))))
  (setf p2 '( (? x) ))
     (format t "~%(miembro ~s ~s): ~s"
    p2 s2 (miembro p2 s2))
)

(defun test-anadir()
  (setf p3 '( P (? y) ))
  (setf s3 '(? x) )
  (setf t3 '(((F A) (? Y)) (B (? X))))
  (format t "~%(anadir ~s ~s): ~s"
  p3 s3 (anadir p3 s3 t3))
)

(defun test-sustituirexpresion ()
  (setf varSustitusora 'j)
  (setf varSustituida '(? x))
  (setf listaTest '((? y) h k ((? x))))
  (format t "~%(sustituirexpresion ~s ~s ~s): ~s"
    varSustituida varSustitusora listaTest (sustituirexpresion varSustitusora varSustituida listaTest))
)

(test)
