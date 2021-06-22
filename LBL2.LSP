;LBL2.LSP - Line/Curve Annotation, Module 1 of 3, resident code.
;
;           February, 1995 by Bill Giel
;
(defun LLBLERR2(s)
    (princ "111")
        (if (/= s "Function cancelled")
                (if (= s "quit / exit abort")
                        (princ)
                        (princ (strcat "\nError: " s))
                )
        )
        (setq *error* lbl_olderr)
        (prin1)
)

(defun LLBLERR(s)
        (if (/= s "Function cancelled")
                (if (= s "quit / exit abort")
                        (princ)
                        (princ (strcat "\nError: " s))
                )
   	)
        (command-s "UCS" "P")
        (setvar "UCSFOLLOW" lbl_oldfollow)
        (setvar "BLIPMODE" lbl_oldblip)
        (setvar "CMDECHO" lbl_oldecho)
        (setvar "OSMODE" lbl_oldosmode)
        (setvar "DIMLFAC" lbl_olddimlfac)
		(setvar "LUPREC" lbl_oldluprec)
        (setvar "DIMPOST" lbl_olddimpost)
        (setvar "TEXTSIZE" lbl_oldthgt)
        
        (vla-EndUndoMark doc)
        (setq *error* lbl_olderr)
        (if (= lbl_mode "L")
                (mapcar '(lambda(x)(set x nil))
                        '(lbl_brgstr lbl_diststr lbl_printstr HBOD HBD BOD DOB BDO BDU
                          DO DU BO BU lbl_getendpts lbl_getline lbl_p1 lbl_p2)
                )
                (mapcar '(lambda(x)(set x nil))
                        '(lbl_getpnts lbl_getarc lbl_arcprint HRLDT HRL RLO RLI ROLI RILO
                          RO RI LO LI lbl_arclen lbl_radlen lbl_radpnt lbl_radius lbl_stangl lbl_pkangl
                          lbl_centangl lbl_angstr lbl_endangl lbl_just lbl_chrdbrg
                          lbl_chrdbrgstr)
                )
        )
        (mapcar '(lambda(x)(set x nil))
            '(lbl_thgt lbl_oldthgt lbl_twid lbl_oldfollow lbl_oldecho lbl_oldblip lbl_olderr
              lbl_twist lbl_oldosmode lbl_olddimlfac lbl_oldluprec lbl_olddimpost lbl_mode LST)
        )
        (prin1)
)

(defun LBLERR(s)
        (if (/= s "Function cancelled")
                (if (= s "quit / exit abort")
                        (princ)
                        (princ (strcat "\nError: " s))
                )
   	)
        (setq *error* lbl_olderr
              lbl_olderr nil)
        (prin1)
)

(defun lbl_getsettings()

    (setq lbl_oldfollow (getvar "UCSFOLLOW")
          lbl_oldecho (getvar "CMDECHO")
          lbl_oldblip (getvar "BLIPMODE")
          mode (strcase mode)
          lbl_twist (getvar "VIEWTWIST")
          lbl_oldosmode (getvar "OSMODE")
          lbl_olddimlfac (getvar "DIMLFAC")
          lbl_oldluprec (getvar "LUPREC")
          lbl_olddimpost (getvar "DIMPOST")
          lbl_oldthgt (getvar "TEXTSIZE")
    )
    (setvar "OSMODE" 0)
    (setvar "CMDECHO" 0)
    (setvar "UCSFOLLOW" 0)
    (setvar "DIMLFAC" (/ 1 12.0))
	(setvar "LUPREC" 2)
    (setvar "DIMPOST" "'")
    (setq *error* LLBLERR)
    (command "UCS" "W")
)

(defun lbl_rtd(a)

	(* 180.0 (/ a pi))
)

(defun lbl_norm(angl)

        (if (< angl 0)(setq angl (+ angl (* pi 2))))
        (if (> angl (* pi 2))(setq angl (- angl (* pi 2))))
        (setq angl angl)
)

(defun lbl_getthgt(/ ta)

    (setq ta (tblsearch "style" (getvar "TEXTSTYLE"))
          lbl_thgt  (cdr (assoc 40 ta))
          lbl_twid (cdr (assoc 41 ta))
    )
    (while (= lbl_thgt 0.0)
        (initget (+ 2 4 64))
        (setq lbl_thgt (getdist (strcat "\nEnter text height <"
                            (rtos (getvar "TEXTSIZE") 2 2)
                            ">:")))
        (if(= lbl_thgt nil)(setq lbl_thgt (getvar "TEXTSIZE")))
        (setq lbl_thgt (* lbl_thgt -1.0))
    )
)

(defun lbl_gettwid(str / len width index ch styl hgt)

    (setq len (strlen str)
          width 0.0
          index 1
          styl (getvar "TEXTSTYLE")
          hgt (abs lbl_thgt)
    )
    (while (<= index len)
        (setq ch (substr str index 1)
              index (1+ index)
        )
        (while (and (= ch "%")(<= index len))
            (setq ch (substr str index 1)
                  index (1+ index)
            )
        )
        (if (= (strcase styl) "MONOTXT")
            (setq width (+ width (* hgt 0.95 lbl_twid)))
            (setq width (cond ((= (ascii ch) 34)(+ width (* hgt 0.67 lbl_twid)))
                              ((= ch "1")(+ width (* hgt 0.67 lbl_twid)))
                              ((= ch "'")(+ width (* hgt 0.36 lbl_twid)))
                              ((= ch ".")(+ width (* hgt 0.36 lbl_twid)))
                              ((= ch "d")(+ width (* hgt 0.36 lbl_twid)))
                              ((= ch "N")(+ width (* hgt 1.15 lbl_twid)))
                              ((= ch "W")(+ width (* hgt 1.15 lbl_twid)))
                              ((= ch "=")(+ width (* hgt 1.25 lbl_twid)))
                              (t (+ width (* hgt 0.95 lbl_twid)))
                        )
            )
        )
    )
    (setq width width)
)

(defun lbl_txtang(string / len index ch newstr)
  (setq len (strlen string)
        index 1
        newstr ""
  )

  (while (<= index len)
    (setq ch (strcase (substr string index 1))
          index (1+ index)
    )
    (if (and (/= ch "%")(/= ch (chr 34)))
      (if (or (= ch "'")(= ch "D"))
         (setq newstr (strcat newstr "-"))
         (setq newstr (strcat newstr ch))
      )
    )
  )
  newstr
)

(defun lbl_getip(/ angl ip)

        (setvar "BLIPMODE" 1)
        (initget 1)
        (setq ip (getpoint "\nInsertion Point: "))
        (if (= ip nil)(exit))

        (if (= lbl_tangl nil)(setq lbl_tangl (lbl_norm (* (getvar "VIEWTWIST") -1))))

        (initget)
        (setq angl (getangle ip (strcat "\nRotation Angle <"
                                        (angtos lbl_tangl)
                                        ">: "))
        )
        (if (/= angl nil)(setq lbl_tangl angl))
        (setvar "BLIPMODE" 0)
        (setq ip ip)
)

(defun BRGLBL( mode )
    (prompt "\nLoading line labelling module...")
    (if (= (load "lbl2a" "FAIL") "FAIL")
        (progn
            (alert "Cannot load module LBL2A.LSP")
            (exit)
        )
    )
    (prompt "OK\n")

    (lbl_getsettings)
    (if (/= mode "LST")(lbl_getthgt))

    (while (= 1 1)
        (if (= lbl_select "Points")
                (lbl_getendpts)
                (lbl_getline)
        )
        (cond ((= mode "HBOD")(HBOD))
              ((= mode "HBD")(HBD))
              ((= mode "BOD")(BOD))
              ((= mode "DOB")(DOB))
              ((= mode "BDO")(BDO))
              ((= mode "BDU")(BDU))
              ((= mode "DO")(DO))
              ((= mode "DU")(DU))
              ((= mode "BO")(BO))
              ((= mode "BU")(BU))
              ((= mode "LST")(LST))
              (t (abort))
        )
    )
)

(defun CURVLBL(mode)

    (prompt "\nLoading curve labelling module...")
    (if (= (load "lbl2b" "FAIL") "FAIL")
        (progn
            (alert "Cannot load module LBL2B.LSP")
            (exit)
        )
    )
    (prompt "OK\n")

    (lbl_getsettings)
    (if (/= mode "LST")(lbl_getthgt))

    (while (= 1 1)
        (if (= lbl_select "Points")
                (lbl_getpnts)
                (lbl_getarc)
        )

        (cond ((= mode "RLO")(RLO))
              ((= mode "RLI")(RLI))
              ((= mode "ROLI")(ROLI))
              ((= mode "RILO")(RILO))
              ((= mode "RO")(RO))
              ((= mode "RI")(RI))
              ((= mode "LO")(LO))
              ((= mode "LI")(LI))
              ((= mode "HRL")(HRL))
              ((= mode "HRLDT")(HRLDT))
              ((= mode "LST")(LST))
              (t (abort))
        )
    )
)

(defun lbl_setmode(key)
    (done_dialog)
    (setq lbl_key key)
    (if (/= key "cancel")
        (setq lbl_mode (strcase (substr key 1 1))
              key (substr key 2 (1- (strlen key)))
        )
        (setq key key)
    )
)
(defun lbl_display_help()
        (acad_helpdlg "lbl2" "")
        (mode_tile "help" 2)
)

(defun lbl_get_styles(/ ta llist lname cstyle)
    (setq ta (tblnext "STYLE" T)
          llist nil
    )
    (while ta
        (setq lname (strcase (cdr (assoc 2 ta)))
              ta (tblnext "STYLE")
        )
        (if (= lname (strcase (getvar "TEXTSTYLE")))
              (setq cstyle lname)
              (setq llist (cons lname llist))
        )
    )
    (setq llist (cons cstyle llist))
    (start_list "style")
    (mapcar 'add_list llist)
    (end_list)
    llist
)




(defun lbl_display_logo(/ dcl_id x y )

        ;LOAD DIALOG BOX RESOURCE FILE
        (setq dcl_id (load_dialog "lbl2.dcl"))
        (if (not (new_dialog "b_logo" dcl_id))(exit))

        (setq x (dimx_tile "logo")
              y (dimy_tile "logo")
        )
        (start_image "logo")
        (fill_image 0 0 x y -15)
        (slide_image 0 0 x y "basis(basis)")
        (end_image)

        (set_tile "text2" "Surveyors' Line and Curve Annotation")
        (set_tile "text3" "BASIS Software Systems")
        (set_tile "text4" "Copyright (C) 1995 by William Giel, LS")
        (set_tile "text5" "All Rights Reserved.")


        ;INITIATE DIALOG
        (start_dialog)
        (unload_dialog dcl_id)

        (mode_tile "info" 2)

)

(defun C:LBL(/ mode dcl_id style_index slist)
    (vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))

    (if (= lbl_select nil)(setq lbl_select "Entities"))
    (if (= lbl_key nil)(setq lbl_key "cancel"))


    (setq lbl_olderr *error*
          *error* LLBLERR2
          dcl_id (load_dialog "lbl2.dcl")
          slist nil
          style_index "0"
    )


    (if (< dcl_id 0)
        (progn
                (alert "Cannot load LBL2.DCL")
                (exit)
        )
    )

    (if (not (new_dialog "lbl" dcl_id))
        (progn
            (alert "Cannot open LBL dialog.")
            (exit)
        )
    )

    (setq slist (lbl_get_styles))

    (foreach x
             '("CRLO" "CRLI" "CROLI" "CRILO" "CRO" "CRI" "CLO"
               "CLI" "CHRL" "CHRLDT" "CLST" "LBOD" "LDOB" "LBDO" "LBDU"
               "LDO" "LDU" "LBO" "LBU" "LHBOD" "LHBD" "LLST" "cancel")
              (action_tile x "(setq mode (lbl_setmode $key))")
    )

    (if (= lbl_select "Points")
            (set_tile "Points" "1")
            (set_tile "Entities" "1")
    )

    (action_tile "Selection" "(setq lbl_select $value)")

    (action_tile "style" "(setq style_index $value)")

    (action_tile "help" "(lbl_display_help)")

    (action_tile "info" "(lbl_display_logo)")

    (mode_tile lbl_key 2)

    (start_dialog)
    (unload_dialog dcl_id)

    (if (/= mode "cancel")
      (progn
        (setvar "TEXTSTYLE" (nth (atoi style_index) slist))
        (if (= lbl_mode "L")
             (brglbl mode)
             (curvlbl mode)
        )
      )
    )
    (vla-EndUndoMark doc)
    (exit)
)

(princ "\nBASIS Line/Curve Annotation loaded.")
(prin1)
