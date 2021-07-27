(load "MergeHatch.lsp" nil)
(load "Utils.lsp" nil)

(defun c:RevitElevLayers (/ *error* main doc layerlist dellist row item qaprev ss)
	(vl-load-com)
	(defun main ()
		(vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))
		
		(setq layerlist (list 
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
		(foreach row layerlist (foreach item row (command "_N" item)))
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

		(foreach row layerlist
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

		(setq basesel (getselbothways))

		(initget "MF UF BF")
		(setq curflr (getkword "What floor is selected? <MF/UF/BF>"))

		(setq dellist (list 
				(list'(8 . "A-FLOR-PATT"))
				(list'(8 . "A-ROOF-PATT"))
				;(list'(8 . "A-FLOR"))
				(list'(8 . "A-AREA"))
				(list'(8 . "S-STRS")'(0 . "HATCH"))
				(list'(8 . "S-FNDN")'(0 . "HATCH"))
			)
			layerlist (list 
				(list (strcat curflr "-HATCH")		(list'(8 . "A-WALL-PATT"))  (list'(8 . "I-WALL-PATT"))  (list'(8 . "I-WALL")'(0 . "HATCH"))  (list'(8 . "A-WALL")'(0 . "HATCH")))
				(list (strcat curflr "-WALLS")		(list'(8 . "I-WALL"))  (list'(8 . "A-WALL"))  (list'(8 . "S-COLS"))  (list'(8 . "S-FNDN"))  (list'(8 . "A-COLS"))  (list'(8 . "A-FLOR")))
				(list (strcat curflr "-OPENING")	(list'(8 . "A-GLAZ"))  (list'(8 . "A-DOOR"))  (list'(8 . "A-GENM"))  (list'(8 . "A-GLAZ-CWMG"))  (list'(8 . "A-GLAZ-CURT")))
				(list (strcat curflr "-APPLIANCE")	(list'(8 . "Q-SPCQ"))  (list'(8 . "M-EQPM"))  (list'(8 . "P-SANR-FIXT")))
				(list (strcat curflr "-CAB")		(list'(8 . "Q-CASE")))
				(list (strcat curflr "-DIM")		(list'(8 . "A-ANNO-DIMS")))
				(list (strcat curflr "-TEXT")		(list'(8 . "G-ANNO-TEXT"))  (list'(8 . "A-AREA-IDEN"))  (list'(8 . "A-GLAZ-IDEN"))  (list'(8 . "A-ANNO-NOTE"))  (list'(8 . "A-DOOR-IDEN"))  (list'(8 . "S-STRS-IDEN"))  (list'(8 . "S-STRS-ANNO"))  (list'(8 . "A-FLOR-HRAL"))  (list'(8 . "G-ANNO-SYMB")))
				(list (strcat curflr "-STAIR")		(list'(8 . "S-STRS"))  (list'(8 . "S-STRS-MBND")))
				(list (strcat curflr "-FURN")		(list'(8 . "I-FURN"))  (list'(8 . "E-LITE-EQPM")))
				(list (strcat curflr "F-BEAM")		(list'(8 . "S-BEAM")))
				(list "RF-PROFILE"					(list'(8 . "A-ROOF"))  (list'(8 . "A-ROOF-OTLN")))
				(list "EL-THIN"						(list'(8 . "A-DETL-MBND"))  (list'(8 . "L-SITE")))
				(list "EL-PLATE"					(list'(8 . "C-TOPO-MAJR"))  (list'(8 . "C-TOPO-MINR"))  (list'(8 . "C-TOPO")))
			)
			modifylist (list
				(list (list'(40 . 7.0))				(list'(40 . 12.0)))
				(list (list'(40 . 4.0))				(list'(40 . 9.0)))
			)
		)

		(hatchfix)

		(command "_.-LAYER")
		(foreach row layerlist (command "_N" (car row)))
		(command "")

		(foreach item dellist
			(selectionLayerChange basesel item nil)
		)
		(foreach row layerlist
			(foreach item (cdr row) (selectionLayerChange basesel item (car row)))
		)
		(foreach row modifylist
			(foreach item (cdr row) (selectionEntModify basesel item (car row)))
		)

		(styles2one "Frank the Architect Upper")
		
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

(defun hatchfix () 
	(if (setq hatches (ssget "_X" (list'(8 . "A-WALL-PATT")'(0 . "HATCH")'(2 . "SOLID"))))
		(progn
			(setq laysel (selectunion basesel laysel))
			(if (not (or (not laysel) (zerop (sslength laysel))))
				(progn
					(setq hatches (hatchsimplifymulti hatches)
						lastEnt (entlast)
					)
					(setvar 'clayer "A-WALL")
					(command "HATCHGENERATEBOUNDARY" hatches "")
					(command "-overkill" (ssget "_X" (list'(8 . "A-WALL"))) "" "" "Done")
					(setvar "qaflags" 1)
					(command "explode" (ssget "_X" (list'(8 . "A-WALL")'(0 . "LWPOLYLINE"))) "")
					(setvar "qaflags" 0)
				)
			)
		)
	)
)


(defun c:fontsize12to7 () (selectionEntModify (getselbothways) (list'(40 . 12.0)) (list'(40 . 7.0))))
(defun c:fontsize9to4 () (selectionEntModify (getselbothways) (list'(40 . 9.0)) (list'(40 . 4.0))))