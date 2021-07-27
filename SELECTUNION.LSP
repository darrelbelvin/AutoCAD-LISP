(defun SSMAP (fun ss / n)
    (if (= 'PICKSET (type ss))
        (repeat (setq n (sslength ss))
            (apply fun (list (ssname ss (setq n (1- n)))))
        )
    )
)

(defun selectunion (sa sb)
    (ssmap '(lambda (e) (if (not (ssmemb e sa)) (ssdel e sb))) sb)
    sb
)

(defun c:selectunion ( / sa sb)
    (if (setq sa (ssget))
        (if (setq sb (ssget))
            (progn
                ;(ssmap '(lambda (e) (if (not (ssmemb e sa)) (ssdel e sb))) sb)
                (sssetfirst nil (selectunion sa sb))
                (command "regen")
            )
        )
    )
    (setq sa nil)
    (setq sb nil)
)


(defun c:su() (c:selectunion))
(defun c:ssu () (command "SELECTSIMILAR") (c:selectunion))