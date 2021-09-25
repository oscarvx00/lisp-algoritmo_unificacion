


;(prog (var1 var2) estas variables son locales
;  cuerpo return
;)

;(defun (var1 var2)
;
;)

(prog unificate (E1, E2)
  (cond
    ((is_atom e1)
      (top e1 e2)
    )
    ((is_atom e2)
      (top e2 e1)
    )
    (t
      ;LA reconcha de su madre
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


(defun anadir (e1 e2)
    (unless (equalp (valorVariable e1) (valorVariable e2)) ; Si e1=e2 no hacer nada
        (if (esVariable e1)
            (if (miembro e1 e2)
                 (return-from anadir 'fallo)
                 (list e2 'barra e1)
            )
            (if (esVariable e2)
                 (list e1 'barra e2)
                 (return-from anadir 'fallo)
            )
        )
    )
)

