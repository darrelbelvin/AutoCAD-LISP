;LBL2B.LSP - Module 3 of 3 for Custom Annotation, transient code
;
;            This module handles curve annotation.
;
;            (C)1995 by Bill Giel

(defun lbl_just(string width / len)
    (if (< (strlen string) width)
        (progn
            (setq len (- width (strlen string)))
            (repeat len
                (setq string (strcat string " "))
            )
        )
        (setq string (substr string 1 (1- width))
              string (strcat string ">")
        )
    )
)


(defun lbl_getpnts(/ unequal stpnt endpnt entlst ename pkpnt arc radcheck)

    (setvar "BLIPMODE" 1)
    (setq unequal 1)
    (while (= unequal 1)
        (initget)
        (setq stpnt (getpoint "\nStarting point of curve: "))
        (if (= stpnt nil)(exit))

        (initget 1)
        (setq endpnt (getpoint "\nEnding point of curve: "))

        (setq entlst nil)
        (while (= entlst nil)
            (setq entlst (entsel "\nPoint on curve: "))
            (if (/= entlst nil)
              (progn
                  (setq ename (car entlst)
                        pkpnt (cadr entlst)
                        arc  (entget ename)
                  )
                  (if (and(/=(cdr (assoc 0 arc)) "ARC")
                                (/=(cdr(assoc 0 arc)) "CIRCLE"))
                      (progn
                          (setq entlst nil)
                          (prompt "\nNot an ARC or CIRCLE entity... try again.")
                      )
                      (setq lbl_radpnt (cdr (assoc 10 arc)))
                  )
              )
            )
        )

       (setq stpnt (list (car stpnt) (cadr stpnt) 0.0)
             endpnt (list (car endpnt) (cadr endpnt) 0.0)
             pkpnt (list (car pkpnt) (cadr pkpnt) 0.0)
             lbl_radpnt (list (car lbl_radpnt) (cadr lbl_radpnt) 0.0)
       )

        (setq radcheck (abs(-(distance lbl_radpnt stpnt)(distance lbl_radpnt endpnt))))
        (if(> radcheck 0.00000001)
                (prompt "\nUnequal Radii...try again.")
                (setq unequal 0)
        )
    )

    (setq lbl_radius (distance lbl_radpnt stpnt)
          lbl_pkangl (angle lbl_radpnt pkpnt)
          lbl_stangl (min (angle lbl_radpnt stpnt)(angle lbl_radpnt endpnt))
          lbl_endangl (max (angle lbl_radpnt stpnt)(angle lbl_radpnt endpnt))
    )

    (if (and (< lbl_pkangl lbl_endangl)(> lbl_pkangl lbl_stangl))
        (setq lbl_centangl (- lbl_endangl lbl_stangl))
        (setq lbl_centangl (- (* pi 2) (- lbl_endangl lbl_stangl)))
    )

    (setq lbl_arclen (strcat "L="
                     (rtos (* (* lbl_radius lbl_centangl)(getvar "DIMLFAC")) 2 (getvar "LUPREC"))
                     (getvar "DIMPOST")
                 )
          lbl_radlen (strcat "R="
                     (rtos (* lbl_radius (getvar "DIMLFAC")) 2 (getvar "LUPREC"))
                     (getvar "DIMPOST")
                 )
    )
    (setq lbl_chrdbrg (lbl_chrdbrgstr stpnt endpnt)
          lbl_chrdbrg (strcat "CB=" lbl_chrdbrg)
    )
    (setvar "BLIPMODE" 0)
)

(defun lbl_getarc(/ entlst ename pkpnt arc )

        (setvar "BLIPMODE" 1)
        (setq entlst nil)
        (while (= entlst nil)
            (setq entlst (entsel "\nPoint on curve: "))
            (if (= entlst nil)(exit))
            (setq ename (car entlst)
                  pkpnt (cadr entlst)
                  arc  (entget ename)
            )
            (if (/=(cdr (assoc 0 arc)) "ARC")
                (progn
                    (setq entlst nil)
                    (prompt "\nNot an ARC... try again.")
                )
                (setq lbl_stangl (min (cdr (assoc 50 arc))(cdr (assoc 51 arc)))
                      lbl_endangl(max (cdr (assoc 50 arc))(cdr (assoc 51 arc)))
                      lbl_radius (cdr (assoc 40 arc))
                      lbl_radpnt (cdr (assoc 10 arc))
                      lbl_pkangl (angle lbl_radpnt pkpnt)
                )
            )
        )
        (if (and (< lbl_pkangl lbl_endangl)(> lbl_pkangl lbl_stangl))
            (setq lbl_centangl (- lbl_endangl lbl_stangl))
            (setq lbl_centangl (- (* pi 2) (- lbl_endangl lbl_stangl)))
        )
        (setq lbl_arclen (strcat "L="
                       (rtos (* (* lbl_radius lbl_centangl)(getvar "DIMLFAC")) 2 (getvar "LUPREC"))
                       (getvar "DIMPOST")
                     )
              lbl_radlen (strcat "R="
                       (rtos (* lbl_radius (getvar "DIMLFAC")) 2 (getvar "LUPREC"))
                       (getvar "DIMPOST")
                     )
        )
        (setq lbl_chrdbrg (lbl_chrdbrgstr
                                  (polar lbl_radpnt lbl_stangl lbl_radius)
                                  (polar lbl_radpnt lbl_endangl lbl_radius)
                          )
              lbl_chrdbrg (strcat "CB=" lbl_chrdbrg)
        )

        (setvar "BLIPMODE" 0)

)

(defun lbl_chrdbrgstr(lbl_p1 lbl_p2 / sec_prec ang s1 s2 degree minute second)
        (setq sec_prec (getvar "AUPREC"))
        (if (<= sec_prec 4)
            (setq sec_prec 0)
            (setq sec_prec (- sec_prec 4))
        )

        (setq ang (+ (- 450.0 (lbl_rtd (angle lbl_p1 lbl_p2)))(lbl_rtd(getvar "userr3")))
              ang (cond ((>= ang 360.0) (- ang 360.0))
                        ((< ang 0.0) (+ ang 360.0))
                        (t ang)
                  )
        )
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

  (strcat s1 degree "-" minute "-" second s2)
)

(defun lbl_arcprint(where str /
                splitangl txtradius width angl index sp rota ch len angl2)

    (if (and (< lbl_pkangl lbl_endangl)(> lbl_pkangl lbl_stangl))
        (setq splitangl (lbl_norm (+ lbl_stangl (/ lbl_centangl 2))))
        (setq splitangl (lbl_norm (- lbl_stangl (/ lbl_centangl 2))))
    )

    (setq angl2 (lbl_norm (+ splitangl lbl_twist)))

    (if(= (strcase where) "OUTSIDE")
        (if (< angl2 pi)
            (setq txtradius (+ lbl_radius (* (abs lbl_thgt) 0.5)))
            (setq txtradius (+ lbl_radius (* (abs lbl_thgt) 1.5)))
        )
        (if (< angl2 pi)
            (setq txtradius (- lbl_radius (* (abs lbl_thgt) 1.5)))
            (setq txtradius (- lbl_radius (* (abs lbl_thgt) 0.5)))
        )
    )
    (setq width (lbl_gettwid str))
    (if (> width (* txtradius lbl_centangl))
        (prompt "\nText is too long for this curve.")
        (progn
            (if (< angl2 pi)
                (setq angl (+ splitangl (/ (/ width 2) txtradius)))
                (setq angl (- splitangl (/ (/ width 2) txtradius)))
            )
            (setq index 1
                  len (strlen str)
            )
            (while (<= index len)
                (setq sp (polar lbl_radpnt angl txtradius)
                      ch (substr str index 1)
                      width (lbl_gettwid ch)
                      index (1+ index)
                )
                (if (< angl2 pi)
                    (setq rota (- angl 1.57079633)
                          angl (- angl (/ width txtradius)))
                    (setq rota (+ angl 1.57079633)
                          angl (+ angl (/ width txtradius)))
                )
                (if (> lbl_thgt 0)
                    (command "TEXT" sp (lbl_rtd rota) ch)
                    (command "TEXT" sp (abs lbl_thgt) (lbl_rtd rota) ch)
                )
            )
        )
    )
)

(defun lbl_angstr(ang / sec_prec degree minute second str)
        (setq sec_prec (getvar "AUPREC"))
        (if (<= sec_prec 4)
            (setq sec_prec 0)
            (setq sec_prec (- sec_prec 4))
        )

        (setq ang (lbl_rtd (lbl_norm ang)))

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

        (setq str (strcat degree "%%d" minute "'" second "\042"))
)

(defun HRL(/ ip)
    (setq ip (lbl_getip))
    (if (> lbl_thgt 0)
        (progn
            (command "TEXT"  ip (lbl_rtd lbl_tangl) lbl_radlen)
            (command "TEXT"  (polar ip (- lbl_tangl (/ pi 2)) (* lbl_thgt 1.5))
                                    (lbl_rtd lbl_tangl) lbl_arclen)
        )
        (progn
            (command "TEXT" ip (abs lbl_thgt) (lbl_rtd lbl_tangl) lbl_radlen)
            (command "TEXT" (polar ip (- lbl_tangl (/ pi 2)) (* (abs lbl_thgt) 1.5))
                                (abs lbl_thgt)(lbl_rtd lbl_tangl) lbl_arclen)
        )
    )
)

(defun HRLDT(/ ip tangent)
    (setq ip (lbl_getip))
    (if (> lbl_thgt 0)
        (command "TEXT"  ip (lbl_rtd lbl_tangl) lbl_radlen)
        (command "TEXT"  ip (abs lbl_thgt) (lbl_rtd lbl_tangl) lbl_radlen)
    )
    (setq ip (polar ip (- lbl_tangl (/ pi 2)) (* (abs lbl_thgt) 1.5)))
    (if (> lbl_thgt 0)
        (command "TEXT" ip (lbl_rtd lbl_tangl) lbl_arclen)
        (command "TEXT" ip (abs lbl_thgt) (lbl_rtd lbl_tangl) lbl_arclen)
    )
    (setq ip (polar ip (- lbl_tangl (/ pi 2)) (* (abs lbl_thgt) 1.5)))
    (command "INSERT" "DELTA" ip (abs lbl_thgt) "" (lbl_rtd lbl_tangl))
    (setq ip (polar ip lbl_tangl (abs lbl_thgt)))
    (if (> lbl_thgt 0)
        (command "TEXT" ip (lbl_rtd lbl_tangl) (strcat "=" (lbl_angstr lbl_centangl)))
        (command "TEXT" ip (abs lbl_thgt) (lbl_rtd lbl_tangl) (strcat "=" (lbl_angstr lbl_centangl)))
    )
    (setq ip (polar ip (+ lbl_tangl pi) (abs lbl_thgt)))
    (setq ip (polar ip (- lbl_tangl (/ pi 2)) (* (abs lbl_thgt) 1.5)))
    (if (/= lbl_centangl pi)
        (setq tangent (/ (sin (/ lbl_centangl 2))(cos (/ lbl_centangl 2)))
              tangent (* (* lbl_radius tangent)(getvar "DIMLFAC"))
              tangent (strcat "T="
                      (rtos tangent 2 (getvar "LUPREC"))(getvar "DIMPOST"))
        )
        (setq tangent "T=(Undefined)")
    )
    (if (> lbl_thgt 0)
        (command "TEXT" ip (lbl_rtd lbl_tangl) tangent)
        (command "TEXT" ip (abs lbl_thgt) (lbl_rtd lbl_tangl) tangent)
    )
)

(defun RLO()
    (lbl_arcprint "OUTSIDE" (strcat lbl_radlen "   " lbl_arclen))
)

(defun RLI()
    (lbl_arcprint "INSIDE" (strcat lbl_radlen "   " lbl_arclen))
)

(defun ROLI()
    (lbl_arcprint "OUTSIDE" lbl_radlen)
    (lbl_arcprint "INSIDE" lbl_arclen)
)

(defun RILO()
    (lbl_arcprint "INSIDE" lbl_radlen)
    (lbl_arcprint "OUTSIDE" lbl_arclen)
)

(defun RO()
    (lbl_arcprint "OUTSIDE" lbl_radlen)
)

(defun RI()
    (lbl_arcprint "INSIDE" lbl_radlen)
)

(defun LO()
    (lbl_arcprint "OUTSIDE" lbl_arclen)
)

(defun LI()
    (lbl_arcprint "INSIDE" lbl_arclen)
)

(defun LST(/ tangent string chord)
  (setq string (strcat "\n " (lbl_just lbl_radlen 25)
                             (lbl_just lbl_arclen 25)
                             (lbl_just (strcat "D=" (lbl_txtang (lbl_angstr lbl_centangl))) 25)
               )
  )
  (prompt string)
  (setq string "")
  (if (/= lbl_centangl pi)
        (setq tangent (/ (sin (/ lbl_centangl 2))(cos (/ lbl_centangl 2)))
              tangent (* (* lbl_radius tangent)(getvar "DIMLFAC"))
              tangent (strcat "T="
                      (rtos tangent 2 (getvar "LUPREC"))(getvar "DIMPOST"))
        )
        (setq tangent "T=(Undefined)")
  )
  (setq chord (strcat "C="
      (rtos (* (* lbl_radius 2)(sin (/ lbl_centangl 2))) 2 (getvar "LUPREC"))
      (getvar "DIMPOST"))
  )

  (setq string (strcat "\n " (lbl_just tangent 25)
                             (lbl_just chord 25)
                             (lbl_just lbl_chrdbrg 25)
               )
  )
  (prompt string)
)
