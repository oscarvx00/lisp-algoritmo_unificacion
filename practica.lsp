(defun unificate (e1 e2)
  (cond
    ((is_atom e1)
      (top e1 e2)
    )
    ((is_atom e2)
      (top e2 e1)
    )
    (t
      (let ((f1 (first e1))
            (f2 (first e2))
            (t1 (rest e1))
            (t2 (rest e2))
            (z1 nil)
            (z2 nil))
            
          (setf z1 (unificate f1 f2))
          (if (equalp z1 'error)
              'error
          )
          (setf g1 (aplicar z1 t1))
          (setf g2 (aplicar z1 t2))
          (setf z2 (unificate g1 g2))
          (if (equalp z2 'error)
              'error
          )
          (setf composicion (componer z1 z2))
          composicion
      )
    )
  )
);fin unificar

;ESPACIO INICIALIZACION
(setf variable-a '(? a))
(setf lista-ab (cons variable-a '(b)))
(setf lista-cd '(c d))

(setf var-z '(? z))
(setf var-y '(? y))
(setf var-g '(? g))
(setf var-x '(? x))

(setf lista-aux-1 (list 'F 'A))
(setf lista-aux-2 (append lista-aux-1 (list var-x)))
(setf lista-aux-3 (cons 'P (list lista-aux-2)))
(setf entrada-1 (append lista-aux-3 (list var-z)))

(setf lista-aux-4 (list var-g 'D))
(setf lista-aux-5 (cons 'P (list var-y)))
(setf entrada-2 (append lista-aux-5 (list lista-aux-4)))

(defparameter e1 '(p (f a x) z))
(defparameter e2 '(p y (w d)))


;(setf arg1 (P (F A (? x)) (? z)))
;(setf arg2 (P (? y) ((? g) D)))
;

(defun top (e1 e2)
  (cond
    ((equalp e1 e2) NIL)
    ((is_var e1) ;si e1 es variable
      (if (miembro e1 e2)
        'error ; return error si es miembro
        (list e2 'barra e1)
      )
    )
    ((is_var e2)
      (list e1 'barra e2)
    )
    (t 'error)   
  )
);fin defun

(defun is_atom (var)
  (cond((atom var) T)
    ((eq (first var) '?) T)
    (t NIL)
  )
)

(defun is_var (var) ; un elemento es variable SI Y SOLO SI es de la forma (? variable)
    (if (atom var) nil ;si es atomo devolver nil
      (if ;si no lo es,
        (and
          (eq (first var) '?)
          (eq (length var) 2)
        ) t  ;y es una lista de len 2 con ? de primer argumento, ES UNA VARIABLE
        nil ;en cualquier otro caso, NO ES VARIABLE
      )
    )
)

(defun miembro (e1 e2)
    (unless (atom e2)
        (member e1 e2)
    )
)


(defun esExpresion (expresion)
    (cond
        ((is_atom expresion) nil)
        (t
            (equalp (first (rest expresion)) 'barra)
        )
    )
)


(defun aplicar (lista expresion)

  (cond
    ((or (null expresion) (is_atom expresion)) lista)
    ((null lista) nil)
    ;; CASOS BASE
    ; A Y/A   Si el item de la lista es igual al ultimo de la expresion devolvemos la parte superior de la expresión
    ((equalp lista (last expresion))
      (first expresion)
    )
    ; (? x) Y/((? x))
    ((is_var lista)
      (if(equalp lista (first (last expresion)))       ;;(? x) Y/(? x)
        (first expresion) ;caso en que la regla cumpla las condiciones para aplicarse, envia la sustitucion
        lista             ;no cambia nada porque no cumple las reglas
      );fin if
    )
    ;;CASOS RECURSIVOS
    ((not (equalp (first (rest expresion)) 'barra)) ;si aun no hemos llegado a la condicion en rest expresion, llamamos recursivamente a la funcion de nuevo
      (aplicar (aplicar lista (first expresion)) (rest expresion))
    )
    ((atom lista) lista)  ;Si la lista es un atomo salimos de la recursividad ya que no podemos avanzar mas niveles
    (t
      (setf parte1 (aplicar (first lista) expresion))
      (setf parte2 (aplicar (rest lista) expresion))
      (if (null parte1)
          parte2
          (if (null parte2)
              parte1
              (list
                  (aplicar (first lista) expresion)
                  (aplicar (rest lista) expresion)
              )
          )
      )
    ); fin default
  );fin cond

)


(defun componer (lista1 lista2)
  (cond
    ((null lista1) lista2)
    ((null lista2) lista1)
    (t
      (cond
        ((and (esExpresion lista1) (esExpresion lista2))
          (if (equalp lista1 lista2)
            lista1
            (list lista1 lista2)
          )
        )
        ((esExpresion lista1)
          (progn
            (setf listaBuena (list (aplicar lista2 (first lista1)) 'barra (first (last lista1))))
            (flatten (list listaBuena (unir (first (last lista1)) lista2)))
          )
        )
        ((esExpresion lista2)
          (progn
            (setf listaBuena (flatten (list (componer (first lista1) lista2) (componer (rest lista1) lista2))))
            (flatten (list listaBuena lista2))
          )
        )
      )
    )
  )
)

; Une los elementos de la composicón tras haber aplicado uno al otro
(defun unir (elemento lista)
    (cond
        ((null lista) nil)
        ((esExpresion lista)
            (if (equalp elemento (first (last lista)))
                    nil
                    lista
            )
        )
        (t
            (let ((parte1 nil)
                  (parte2 nil))
                (setf parte1 (unir elemento (first lista)))
                (setf parte2 (unir elemento (rest lista)))
                (if (null parte1)
                    (if (null parte2)
                        nil
                        parte2
                    )
                    (if (null parte2)
                        parte1
                        (flatten (list parte1 parte2))
                    )
                )
            )
        )
    )
)

; Función que elimina paréntesis innecesarios
(defun flatten (l)
    (cond ((null l) nil)
        ((atom l) (list l))
        ((esExpresion l) (list l))
        (t
            (loop for a in l appending (flatten a))
        )
    )
)
