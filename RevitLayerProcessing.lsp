(defun c:RevitElevLayers (/ *error* main doc thelist dellist row item qaprev ss)
	(vl-load-com)
	(defun main ()
		(vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))
		
		(setq thelist (list 
			'("EL-PLATE" 		"A-FLOR-LEVL" "I-WALL" "A-GLAZ-HDLN" "A-DETL-GENF")
			'("EL-TEXT"			"G-ANNO-DIMS" "G-ANNO-TTLB" "G-ANNO-TTLB-WIDE" "G-ANNO-NPLT" "G-IMPT")
			'("EL-THIN"			"A_ROOF_MCUT" "M-EQPM" "A-DOOR" "A-DOOR-FRAM" "E-LITE-EQPM" "A-DOOR-GLAZ" "A-FLOR-HRAL" "A-GENM" "A-GLAZ" "S-BEAM" "A-DETL-MEDM")
			'("EL-MED"			"S-FNDN" "A-FLOR" "L-SITE" "S-STRS" "A-ROOF-MCUT")
			'("EL-OUTLINE"		"A-ROOF" "A-WALL" "A-COLS" "A-ROOF-OTLN" "C-BLDG-PADS" "C-TOPO" "A-FLOR-MCUT")
			'("EL-DIM"			"A-ANNO-DIMS" "A-ANNO-DIMS-96")
			'("EL-HATCH"		"A-FLOR-PATT" "A-WALL-PATT")
			'("EL-SHADOWS"		"A-ROOF-PATT" "I-FURN" "CTOPO-MAJR" "CTOPO-MINR")
			)
			dellist '("G-ANNO-SYMB" "A-AREA" "G-ANNO-REFR")
		)

		(command "_.-LAYER")
		(foreach item dellist (command "_N" item))
		(foreach row thelist (foreach item row (command "_N" item)))
		(command "")

		(setq qaprev (getvar "qaflags"))
		(setvar "qaflags" 1)
		(if (setq ss (ssget "_X" (list'(0 . "INSERT"))))
			(command "_.EXPLODE" ss "")
		)
		(if (setq ss (ssget "_X" (list'(0 . "INSERT"))))
			(command "_.EXPLODE" ss "")
		)
		(setvar "qaflags" qaprev)
		
		(setq ss (ssget "X" '((67 . 0))))

		(selectionLayerChange ss (list'(0 . "HATCH")'(8 . "A-GLAZ")) "EL-SHADOWS")
		(selectionLayerChange ss (list'(0 . "HATCH")'(8 . "C-TOPO")) nil)

		(foreach row thelist
			(foreach item (cdr row) (selectionLayerChange ss (list (cons 8 item)) (car row)))
		)
		(foreach item dellist
			(selectionLayerChange ss (list (cons 8 item)) nil)
		)

		(setq ss nil)
		(command "PURGE" "A" "*" "N")

		(vla-EndUndoMark doc)
		(princ)
	)
	(defun *error*(s)
		(princ s)
		(vla-EndUndoMark doc)
		(princ)
	)
	(main)
)

(defun c:RevitPlanLayers (/ *error* main doc curflr basesel)
	(vl-load-com)
	(defun main ()
		(c:ColorByBlockAll)
		(vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))

		(if (not (setq basesel (ssget "_I"))); if there is not an Implied [= pre-] selection [if there is, it will put that into 'ss']
			(setq basesel (ssget)); then -- ask the User to select
		); if

		(initget "MF- UF- BF-")
		(setq curflr (getkword "What floor is selected? <MF-/UF-/BF->"))

		(setq thelist (list 
			(list (strcat curflr "HATCH")		(list'(8 . "A-WALL-PATT"))  (list'(8 . "I-WALL-PATT"))  (list'(8 . "I-WALL")'(0 . "HATCH"))  (list'(8 . "A-WALL")'(0 . "HATCH")))
			(list (strcat curflr "WALLS")		(list'(8 . "I-WALL"))  (list'(8 . "A-WALL"))  (list'(8 . "S-COLS")))
			(list (strcat curflr "OPENING")		(list'(8 . "A-GLAZ"))  (list'(8 . "A-DOOR"))  (list'(8 . "A-GENM")))
			(list (strcat curflr "APPLIANCE")	(list'(8 . "Q-SPCQ"))  (list'(8 . "M-EQPM"))  (list'(8 . "P-SANR-FIXT")))
			(list (strcat curflr "CAB")			(list'(8 . "Q-CASE")))
			(list (strcat curflr "DIM")			(list'(8 . "A-ANNO-DIMS")))
			(list (strcat curflr "TEXT")		(list'(8 . "G-ANNO-TEXT"))  (list'(8 . "A-AREA-IDEN"))  (list'(8 . "A-GLAZ-IDEN"))  (list'(8 . "A-ANNO-NOTE"))  (list'(8 . "A-DOOR-IDEN"))  (list'(8 . "S-STRS-IDEN"))  (list'(8 . "S-STRS-ANNO"))  (list'(8 . "A-FLOR-HRAL"))  (list'(8 . "G-ANNO-SYMB")))
			(list (strcat curflr "STAIR")		(list'(8 . "S-STRS"))  (list'(8 . "S-STRS-MBND")))
			(list (strcat curflr "FURN")		(list'(8 . "I-FURN"))  (list'(8 . "E-LITE-EQPM")))
			(list "RF-PROFILE"					(list'(8 . "A-ROOF"))  (list'(8 . "A-ROOF-OTLN")))
			(list "EL-THIN"						(list'(8 . "A-DETL-MBND"))  (list'(8 . "L-SITE")))
			)
			dellist (list (list'(8 . "A-FLOR-PATT"))  (list'(8 . "A-FLOR"))  (list'(8 . "A-AREA")))
		)
		
		(command "_.-LAYER")
		(foreach row thelist (command "_N" (car row)))
		(command "")

		(foreach row thelist
			(foreach item (cdr row) (selectionLayerChange basesel item (car row)))
		)
		(foreach item dellist
			(selectionLayerChange basesel item nil)
		)

		(setallstylesfont "Frank the Architect Upper.ttf")
		
		(vla-EndUndoMark doc)
		(princ)
	)
	(defun *error*(s)
		(princ s)
		(vla-EndUndoMark doc)
		(princ)
	)
	(main)
)

(defun selectionLayerChange (basesel filter targetLayer)
	(if (setq laysel (ssget "_X" filter))
		(progn
			(setq laysel (selectunion basesel laysel))
			(if (or (not laysel)
				(zerop (sslength laysel)))
				(princ "No selection")
				(if (null targetLayer)
					(command "_.erase" laysel "")
					(command "_.change" laysel "" "_Properties" "_LAyer" targetLayer "")
				)
			)
		)
	)
	(setq laysel nil)
)

(defun c:colorByBlockAll (/ *error* main doc color entity block)
	(vl-load-com)
	(defun main ()
		(vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))

		(setq color (vlax-create-object (strcat "AutoCAD.AcCmColor." (substr (getvar "acadver") 1 2))))
		(vla-put-colorIndex color AcByBlock)
		(setq colorLayer (vlax-create-object (strcat "AutoCAD.AcCmColor." (substr (getvar "acadver") 1 2))))
		(vla-put-colorIndex colorLayer acByLayer)

		(vlax-for block (vla-get-blocks
								(vla-get-activedocument
									(vlax-get-acad-object)
								)
							)
			(if (and (= (vla-get-IsLayout block) :vlax-false)
				(= (vla-get-IsXref block) :vlax-false)
				)
				(vlax-for entity block
					(vla-put-trueColor entity color)
					(vla-put-layer entity "0" )
					(vlax-release-object entity)
				); vlax-for
				(vlax-for entity block
					(vla-put-trueColor entity colorLayer)
					(vlax-release-object entity)
				); vlax-for
			)
		); vlax-for
		
		
		(vla-EndUndoMark doc)
		(princ)
	)
	(defun *error*(s)
		(princ s)
		(vla-EndUndoMark doc)
		(princ)
	)
	(main)
)

(defun setallstylesfont	(font_file / d)
  (vlax-for style (vla-get-textstyles (setq d (vla-get-activedocument (vlax-get-acad-object))))
    (vla-put-fontfile style font_file)
  )
  (vla-regen d acactiveviewport)
)

; (cond
; 		((eq (setq blkName (getstring "\nBlock name: ")) "") 
; 			nil
; 			)
; 		((null (tblsearch "block" blkName))
; 			(vlr-beep-reaction)
; 			(prompt (strcat "\nblock " blkName " is not found."))
; 		)
; 		( t