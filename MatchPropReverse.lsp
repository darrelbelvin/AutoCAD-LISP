(defun c:matchpropreverse( / en ss )
    (if
        (and
            (setq ss (ssget "_:L"))
            (progn (setvar 'ERRNO 0)
                (while
                    (not
                        (or
                            (= 52 (getvar 'ERRNO))
                            (setq en (car (entsel "\nSelect source object: ")))
                        )
                    )
                    (princ "\nNothing Selected.")
                )
                en
            )
        )
        (command "_.matchprop" en ss "")
    )
    (princ)
)

(defun c:qr() (c:matchpropreverse))