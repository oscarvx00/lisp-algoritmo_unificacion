(defun unificate-init (e1 e2)

    (let ((u nil))
        (if (eql (first e1) (first e2)) ; Comprueba que el primer elemento de ambas expresiones es el mismo
            (unificate e1 e2 u)
        )
    )
)

(defun unificate (e1 e2 u)

    (cond
        ((atomp e1) ;equivalente al antiguo is_atom
            (top e1 e2 u)
        )
        ((atomp e2)
            (top e2 e1 u)
        )
        (T
            (setf u (unificate (do-subst (first e1) u) (do-subst (first e2) u) u))
            (if (equalp u 'error) 
                'error
            )
            (unificate (rest e1) (rest e2) u)
        )
    )
)



(defun atomp (s)

    (cond((atom s) T)
    ((eq (first s) '?) T)
    (t NIL)
  )

)

(defun top (e1 e2 u)
  (cond
    ((equalp e1 e2) NIL)
    ((variablep e1) ;si e1 es variable
      (if (miembro e1 e2)
        'error ; return error si es miembro
        (add-pair e2 e1 u)
      )
    )
    ((variablep e2)
      (add-pair e1 e2 u)
    )
    (t 'error)   
  )
);fin defun

(defun add-pair (term variable u)
  "Trys to add the pair (TERM VARIABLE) to the list U and
   return the new list.  Otherwise throws NOT-UNIFIABLE."
    (cons (list term variable) 
                (subst1 term variable u))
)


(defun miembro (e1 e2)
    (unless (atom e2)
        (member e1 e2)
    )
)

(defun variablep (s)
  "Returns T if S is a known variable."
  (if (atom s) nil ;si es atomo devolver nil
      (if ;si no lo es,
        (and
          (eq (first s) '?)
          (eq (length s) 2)
        ) t  ;y es una lista de len 2 con ? de primer argumento, ES UNA VARIABLE
        nil ;en cualquier otro caso, NO ES VARIABLE
      )
    )
  ;(member s '(x y z x1 x2 y1 y2 z1 z2 u v w)) )
)

(defun do-subst (exp l)
  "Applies the substitutions of L to EXP."
  (cond ((null l) exp)
        (t (subst1 (first (first l))
                   (second (first l))
                   (do-subst exp (rest l)) )) ) )

(defun subst1 (a b lst)
  "Substitutes A for each occurrence of B in LST."
    (cond
       ((null lst) nil)
       ((eql lst b) a)
       ((atom lst) lst)
       ((eql b (first lst))
        (cons a (subst1 a b (rest lst))) )
       ((atom (first lst))
        (cons (first lst)(subst1 a b (rest lst))) )
       (t (cons (subst1 a b (first lst))
                (subst1 a b (rest lst)) )) ) )














;;; UNIFY.CL
;;; A Unification algorithm for literals in the Predicate Calculus,
;;; implemented in Common Lisp.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 6 ("Logical Reasoning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Note: This function UNIFY does indeed perform the "occurs check"
;;; required by the strict definition of unification.

;;; UNIFY is the top-level function.
(defun unificate-init (e1 e2)

    (let ((u nil))
        (if (eql (first e1) (first e2))
            (unificate e1 e2 u)
        )
    )
)

(defun miembro (e1 e2)
    (unless (atom e2)
        (member e1 e2)
    )
)

(defun unificate (e1 e2 u)

    (cond
        ((atomp e1)
            (top e1 e2 u)
        )
        ((atomp e2)
            (top e2 e1 u)
        )
        (T
            (setf u (unificate (do-subst (first e1) u) (do-subst (first e2) u) u))
            (if (equalp u 'error) 
                'error
            )
            (unificate (rest e1) (rest e2) u)
        )
    )
)



(defun atomp (s)

    (cond((atom s) T)
    ((eq (first s) '?) T)
    (t NIL)
  )

)

(defun top (e1 e2 u)
  (cond
    ((equalp e1 e2) NIL)
    ((variablep e1) ;si e1 es variable
      (if (miembro e1 e2)
        'error ; return error si es miembro
        (add-pair e2 e1 u)
      )
    )
    ((variablep e2)
      (add-pair e1 e2 u)
    )
    (t 'error)   
  )
);fin defun

(defun add-pair (term variable u)
  "Trys to add the pair (TERM VARIABLE) to the list U and
   return the new list.  Otherwise throws NOT-UNIFIABLE."
    (cons (list term variable) 
                (subst1 term variable u))
)

;;; DO-SUBST performs all substitutions in L on EXP in
;;; reverse order.
(defun do-subst (exp l)
  "Applies the substitutions of L to EXP."
  (cond ((null l) exp)
        (t (subst1 (first (first l)) ; (first l)= ((sustitusor)(sustituyendo))
                   (second (first l))
                   (do-subst exp (rest l)) )) ) )

(defun subst1 (a b lst)
  "Substitutes A for each occurrence of B in LST."
    (cond
       ((null lst) nil)
       ((eql lst b) a)
       ((atom lst) lst)
       ((eql b (first lst))
        (cons a (subst1 a b (rest lst))) )
       ((atom (first lst))
        (cons (first lst)(subst1 a b (rest lst))) )
       (t (cons (subst1 a b (first lst))
                (subst1 a b (rest lst)) )) ) )

(defun occurs-in (elt exp)
  "Returns T if ELT occurs in EXP at any level. Comprueba que elt pertenezca a exp"
  (cond ((eql elt exp) t)
        ((atom exp) nil)
        (t (or (occurs-in elt (first exp))
               (occurs-in elt (rest exp)) )) ) ) ;T si elt pertenece a exp

(defun variablep (s)
  "Returns T if S is a known variable. Es el equivalente al antiguo is_var"
  (if (atom s) nil ;si es atomo devolver nil
      (if ;si no lo es,
        (and
          (eq (first s) '?)
          (eq (length s) 2)
        ) t  ;y es una lista de len 2 con ? de primer argumento, ES UNA VARIABLE
        nil ;en cualquier otro caso, NO ES VARIABLE
      )
    )
  ;(member s '(x y z x1 x2 y1 y2 z1 z2 u v w)) )
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

;;; Here's a function for demonstrating UNIFY.
(defun show-unification (lit1 lit2)
  "Prints out both inputs and output from UNIFY."
  (format t "~%Result of UNIFY on ~s and ~s is ~s."
          lit1 lit2 (unificate-init lit1 lit2) ) )

(defun test ()
  "Calls UNIFY with sample arguments."
  (show-unification *literal1* *literal2*)
  (show-unification *literal3* *literal4*)
  (show-unification *literal5* *literal6*)
  (show-unification *literal7* *literal8*)
  (let ((u (unificate-init *literal7* *literal8*)))
    (format t "~%Result of applying U to ~s is ~s."
            *literal7* (do-subst *literal7* u) )
    (format t "~%Result of applying U to ~s is ~s."
            *literal8* (do-subst *literal8* u) )
  ) )

(test)