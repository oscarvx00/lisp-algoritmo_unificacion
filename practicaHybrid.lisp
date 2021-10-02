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
    (t (or (miembro entrada (first lista))
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
       ; Now unify the rest of each.
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


;;; Here is some test data:
(defparameter *literal1* '(p (? x) (f a)))
(defparameter *literal2* '(p b (? y)))
(defparameter *literal3* '(p (f (? x)) (g a (? y))))
(defparameter *literal4* '(p (f (h b)) (g (? x) (? y))))
(defparameter *literal5* '(p (? x)))
(defparameter *literal6* '(p (f (? x))))
(defparameter *literal7* '(p (? x) (f (? y)) (? x)))
(defparameter *literal8* '(p (? z) (f (? z)) a))

(defparameter e1 '(p (f a (? x)) (? z)))
(defparameter e2 '(p (? y) ((? w) d)))

;;; Here's a function for demonstrating UNIFY.
(defun show-unification (lit1 lit2)
  "Prints out both inputs and output from UNIFY."
  (format t "~%Result of UNIFY on ~s and ~s is ~s."
    lit1 lit2 (unificar-init lit1 lit2)
  )
)

(defun test ()
  "Calls UNIFY with sample arguments."
  (show-unification *literal1* *literal2*)
  (show-unification *literal3* *literal4*)
  (show-unification *literal5* *literal6*)
  (show-unification *literal7* *literal8*)
  (show-unification e1 e2)

  (let ((theta (unificar-init *literal7* *literal8*)))
    (format t "~%Result of applying U to ~s is ~s."
            *literal7* (aplicarsustitucion *literal7* theta) )
    (format t "~%Result of applying U to ~s is ~s."
            *literal8* (aplicarsustitucion *literal8* theta) )
  ) )
  

(test)
