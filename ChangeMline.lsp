; BeekeeCZ 2015/09
; Change MLine Style

(vl-load-com)

(defun c:CHMLS ( / *error* LM:UniqueFuzz nVAR oVAR adoc ss edo edn a tmp vrts)

  ; ----
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break,end"))
      (princ (strcat "\nError: " errmsg)))
    (mapcar 'setvar nVAR oVAR)
    (vla-endundomark adoc)
    (princ))

  ;----- Lee Mac, http://www.lee-mac.com/uniqueduplicate.html
  (defun LM:UniqueFuzz (l f)
    (if l (cons (car l)
		(LM:UniqueFuzz (vl-remove-if (function (lambda ( x ) (equal x (car l) f))) (cdr l))
		               f))))
  
  ; ----
  ; ----
  
  (vla-startundomark (setq adoc (vla-get-activedocument (vlax-get-acad-object))))
  (setq oVAR (mapcar 'getvar (setq nVAR '(CMDECHO OSMODE ORTHOMODE SNAPMODE BLIPMODE CMLSTYLE CMLJUST CMLSCALE))))
  
  
  (if (and (princ "\nNeed MLINEs,")
           (setq ss (ssget ":L" '((0 . "MLINE"))))
           (setq #nstyle (if (= "" (setq tmp (getstring (strcat "\nChange style to <" (cond (#nstyle) ("N/A")) ">: "))))
                           (cond (#nstyle)
                                 (nil))
                           tmp))
           (dictsearch (cdr (assoc -1 (dictsearch (namedobjdict) "ACAD_MLINESTYLE"))) #nstyle)
           )
    (progn
      (repeat (setq i (sslength ss))
        
        (setq edo (entget (ssname ss (setq i (1- i))))
              vrts (LM:UniqueFuzz (mapcar 'cdr (vl-remove-if-not '(lambda (x) (member (car x) '(10 11))) edo)) 0.001))
        
        (mapcar 'setvar nVAR (list 1 0 0 0 0 #nstyle (cdr (assoc 70 edo)) (cdr (assoc 40 edo))))
        (command "_.MLINE") (apply 'command vrts) (command)
        (setq edn (entget (entlast)))
        
        (entmod (foreach e (vl-remove 'nil (mapcar '(lambda (x) (assoc x edo)) '(6 8 62 39 43 48 370 71)))
                  (setq edn (if (setq a (assoc (car e) edn))
                              (subst  e
                                      a
                                      edn)
                              (append edn
                                      (list e))))))
      )
      (command "_.ERASE" ss ""))
    (cond ((not ss)
           (princ "No MLINEs selected."))
          ((not (dictsearch (cdr (assoc -1 (dictsearch (namedobjdict) "ACAD_MLINESTYLE"))) #nstyle))
           (princ (strcat #nstyle " mline style does not exists/loaded."))
           (setq #nstyle nil)))
    )
  (*error* "end")
)

; (ssget '((0 . "MLINE") (2 . "4WALL")))