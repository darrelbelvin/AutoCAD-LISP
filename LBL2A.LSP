;LBL2A.LSP - Module 2 of 3 for Custom Annotation, transient code.
;
;            This module handles line annotation.
;
;            (C)1995 by Bill Giel

(defun lbl_getendpts()
    (setvar "BLIPMODE" 1)
    (initget)
    (setq lbl_p1 (getpoint "\nSelect first point: "))
    (if (= lbl_p1 nil)(exit))

    (initget 1)
    (setq lbl_p2 (getpoint "\nSelect second point: "))

    (setq lbl_p1 (list (car lbl_p1) (cadr lbl_p1) 0.0)
          lbl_p2 (list (car lbl_p2) (cadr lbl_p2) 0.0)
    )
    (setvar "BLIPMODE" 0)
)


(defun lbl_getline(/ entlst ename pkpnt line tp1 tp2)

    (setq entlst nil)
    (while (= entlst nil)
        (setvar "BLIPMODE" 1)
        (setq entlst (entsel "\nPoint on Line: "))
        (if (= entlst nil) (exit))
        (setvar "BLIPMODE" 0)
        (setq ename (car entlst)
              pkpnt (cadr entlst)
              line  (entget ename)
        )
        (if (/=(cdr (assoc 0 line)) "LINE")
            (progn
                (setq entlst nil)
                (prompt "\nNot a LINE... try again.")
            )
            (progn
                (setq tp1 (cdr (assoc 10 line))
                      tp2 (cdr (assoc 11 line))
                )
                (setq tp1 (list (car tp1) (cadr tp1) 0.0)
                      tp2 (list (car tp2) (cadr tp2) 0.0)
                      pkpnt (list (car pkpnt) (cadr pkpnt) 0.0)
                )
                (if (< (distance pkpnt tp1)(distance pkpnt tp2))
                    (setq lbl_p1 tp1
                          lbl_p2 tp2)
                    (setq lbl_p1 tp2
                          lbl_p2 tp1)
                )
            )
        )
    )
)

(defun lbl_brgstr(/ sec_prec ang s1 s2 degree minute second)
        (setq sec_prec (getvar "AUPREC"))
        (if (<= sec_prec 4)
            (setq sec_prec 0)
            (setq sec_prec (- sec_prec 4))
        )

        (setq ang (+ (- 450.0 (lbl_rtd (angle lbl_p1 lbl_p2)))(lbl_rtd(getvar "userr3"))))
        (while (>= ang 360.0)(setq ang (- ang 360.0)))
        (while (< ang 0.0)(setq ang (+ ang 360)))
        (cond
            ((and (>= ang 0.0)(<= ang 90.0))
                (setq s1 "N "
                      s2 " E"
                )
            )
            ((and (> ang 90.0)(<= ang 180.0))
                (setq s1 "S "
                      s2 " E"
                      ang (- 180.0 ang)
                )
            )
            ((and (> ang 180.0)(< ang 270.0))
                (setq s1 "S "
                      s2 " W"
                      ang (- ang 180.0)
                )
            )
            ((and (>= ang 270.0)(< ang 360.0))
                (setq s1 "N "
                      s2 " W"
                      ang (- 360.0 ang)
                )
            )
        )
        (setq degree (itoa (atoi (rtos ang 2 8)))
	      ang (* (- ang (atof degree)) 60.0)
	      minute (itoa (atoi (rtos ang 2 8)))
	      ang (* (- ang (atof minute)) 60.0)
              second (rtos (abs ang) 2 sec_prec)
        )

        (if (= (atoi second) 60)
			(setq second "0"
			      minute (itoa (+ (atoi minute) 1)))
	)

	(if (= (atoi minute) 60)
			(setq minute "0"
			      degree (itoa (+ (atoi degree) 1)))
        )


	(if (< (atoi minute) 10)(setq minute (strcat "0" minute)))
	(if (< (atoi second) 10)(setq second (strcat "0" second)))
	(strcat s1 degree "%%d" minute "'" second "\042" s2)
)

(defun lbl_diststr()
    (strcat (rtos (* (distance lbl_p1 lbl_p2)(getvar "DIMLFAC")) 2 (getvar "LUPREC"))
            (getvar "DIMPOST")
    )
)

(defun lbl_printstr(where string percent / angl p1a p2a p3)
    (setq angl (angle lbl_p1 lbl_p2))
    (if(and(>(lbl_norm (+ angl lbl_twist))(/ pi 2))(<(lbl_norm(+ angl lbl_twist))(*(/ pi 2)3)))
                (setq p1a lbl_p2
                      p2a lbl_p1)
                (setq p1a lbl_p1
                      p2a lbl_p2)
    )
    (setq angl (angle p1a p2a)
          p3 (polar p1a angl (* (distance p1a p2a) percent))
    )
    (if (= (strcase where) "OVER")
        (setq angl (+ angl (/ pi 2)))
        (setq angl (- angl (/ pi 2)))
    )
    (setq p3 (polar p3 angl (* (abs lbl_thgt) 0.875))
          angl (angle p1a p2a)
    )
    (if (> lbl_thgt 0)
        (command "TEXT" "M" p3 (lbl_rtd angl) string)
        (command "TEXT" "M" p3 (abs lbl_thgt) (lbl_rtd angl) string)
    )
)

(defun HBOD(/ brg dist ip)
        (setq brg (lbl_brgstr)
              dist (lbl_diststr)
        )
        (setq ip (lbl_getip))
        (if (> lbl_thgt 0)
            (progn
                (command "TEXT" "M" ip (lbl_rtd lbl_tangl) brg)
                (command "TEXT" "M" (polar ip (- lbl_tangl (/ pi 2)) (* lbl_thgt 1.5))
                                        (lbl_rtd lbl_tangl) dist)
            )
            (progn
                (command "TEXT" "M" ip (abs lbl_thgt) (lbl_rtd lbl_tangl) brg)
                (command "TEXT" "M" (polar ip (- lbl_tangl (/ pi 2)) (* (abs lbl_thgt) 1.5))
                                    (abs lbl_thgt)(lbl_rtd lbl_tangl) dist)
            )
        )
)

(defun HBD(/ brg dist ip)

        (setq brg (lbl_brgstr)
              dist (lbl_diststr)
        )
        (setq ip (lbl_getip))
        (if (> lbl_thgt 0)
                (command "TEXT" "M" ip (lbl_rtd lbl_tangl) (strcat brg ", " dist))
                (command "TEXT" "M" ip (abs lbl_thgt) (lbl_rtd lbl_tangl) (strcat brg ", " dist))
        )
)

(defun BOD()
    (lbl_printstr "OVER" (lbl_brgstr) 0.50)
    (lbl_printstr "UNDER" (lbl_diststr) 0.50)
)

(defun DOB()
    (lbl_printstr "UNDER" (lbl_brgstr) 0.50)
    (lbl_printstr "OVER" (lbl_diststr) 0.50)
)

(defun BDO()
    (lbl_printstr "OVER" (lbl_brgstr) 0.33)
    (lbl_printstr "OVER" (lbl_diststr) 0.66)
)

(defun BDU()
    (lbl_printstr "UNDER" (lbl_brgstr) 0.33)
    (lbl_printstr "UNDER" (lbl_diststr) 0.66)
)

(defun DO()
    (lbl_printstr "OVER" (lbl_diststr) 0.50)
)

(defun DU()
    (lbl_printstr "UNDER" (lbl_diststr) 0.50)
)

(defun BO()
    (lbl_printstr "OVER" (lbl_brgstr) 0.50)
)

(defun BU()
    (lbl_printstr "UNDER" (lbl_brgstr) 0.50)
)

(defun LST()
    (prompt (strcat "\n" (lbl_txtang (lbl_brgstr)) ", " (lbl_diststr)))
)
